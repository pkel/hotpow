(* {{{ *)
open Common

module Poll : sig
  (** Helper module for counting votes and assembling quorums. *)

  type t

  val count : t -> vote -> bool
  (** [count t vote] adds [vote] to the store and returns true if vote improved
      progress. *)

  val lead : ?replace:bool -> t -> block Link.t -> quorum option
  (** [lead t lnk] checks for leadership on [lnk] and returns corresponding
      quorum. Per default, this does leader replacement by omitting small
      foreign votes. The leader replacement can be deactivated with
      [~replace:false]. *)

  val progress : t -> block Link.t -> int
  (** [progress t lnk] maps the progress on a quorum for [lnk] to integer
      scale for comparison. The higher the output, the more likely it is that
      [lnk] will make it. *)

  val gc : keep:(block Link.t -> bool) -> t -> unit
  (** Garbage collection. [gc ~keep t] drops all votes for references from [t],
      for which [keep lnk] yields [false]. *)

  val create : q:quorum_config -> DSA.public_key -> t
  (** [create ~q id] sets up a vote store that produces quorums for the given
      [id]. *)
end = struct
  module Elt = struct
    type t = {weight: int; prefer: bool; id: DSA.public_key; s: solution}

    let compare a b =
      let w = compare a.weight b.weight in
      if w != 0 then w
      else
        let p = compare a.prefer b.prefer in
        if p != 0 then p else compare (a.id, a.s) (b.id, b.s)
  end

  open Elt

  (* Partial quorum is stored as set. *)
  module Set = Set.Make (Elt)

  (* We keep foreign and own votes separate and merge when needed. *)
  type ht_val = {my: Set.t; other: Set.t}

  type t =
    {ht: (block Link.t, ht_val) Hashtbl.t; q: quorum_config; me: DSA.public_key}

  let count t ((lnk, id, s) as vote) =
    (* get from mutable store, default to empty votes *)
    let votes =
      match Hashtbl.find_opt t.ht lnk with
      | None ->
          let votes = Set.{my= empty; other= empty} in
          Hashtbl.add t.ht lnk votes ; votes
      | Some x -> x
    in
    (* count as own or foreign vote *)
    let is_my = id = t.me in
    let elt = {weight= Weight.weigh vote; id; s; prefer= is_my} in
    let set = if is_my then votes.my else votes.other in
    let set', freshness =
      if Set.mem elt set then (set, false)
      else
        let set = Set.add elt set in
        if Set.cardinal set > t.q.size then
          let max = Set.max_elt set in
          (Set.remove max set, max != elt)
        else (set, true)
    in
    let votes' =
      if is_my then {votes with my= set'} else {votes with other= set'}
    in
    let () = Hashtbl.replace t.ht lnk votes' in
    freshness

  let gc ~keep t =
    Hashtbl.filter_map_inplace
      (fun k v -> if keep k then Some v else None)
      t.ht

  (* Take leadership by omitting others' votes *)
  let lead ?(replace = true) t lnk =
    match Hashtbl.find_opt t.ht lnk with
    | Some votes -> (
      (* no vote -> no lead *)
      match Set.min_elt_opt votes.my with
      | None -> None
      | Some my_best -> (
          (* drop small votes of honest nodes for leader replacement *)
          let other =
            if replace then
              let _, _, above = Set.split my_best votes.other in
              above
            else votes.other
          in
          (* Consume merged votes into quorum of size t.q.size *)
          let rec f n sum acc seq =
            if n = t.q.size then Some (List.rev acc)
            else
              match seq () with
              | Seq.Nil -> None
              | Cons (elt, tl) ->
                  let sum = sum + elt.weight in
                  if sum <= t.q.threshold * t.q.size then
                    f (n + 1) sum ((elt.id, elt.s) :: acc) tl
                  else None
          in
          f 0 0 [] Set.(to_seq (union votes.my other))
          |>
          (* for replace=false we have to make sure that we lead *)
          if replace then fun x -> x
          else function
            | None -> None
            | Some ((id, _) :: _ as q) -> if id = t.me then Some q else None
            | Some _l -> raise (Failure "produced empty quorum") ) )
    | _ -> None

  let progress t lnk =
    match Hashtbl.find_opt t.ht lnk with
    | None -> 0
    | Some votes ->
        let rec f i acc = function
          | Seq.Nil -> i
          | Cons (e, seq) ->
              let sum = acc + e.weight in
              if sum <= (i + 1) * t.q.threshold then f (i + 1) sum (seq ())
              else i
        in
        f 0 0 Set.(to_seq (union votes.my votes.other) ())

  let create ~q me = {ht= Hashtbl.create 5; q; me}
end

module BlockStore : sig
  type 'a t
  type 'a key = {parent: 'a -> block Link.t; this: 'a -> block Link.t}

  val create : 'a key -> 'a t
  val get : 'a t -> block Link.t -> 'a option
  val add : 'a t -> 'a -> unit
  val rm : 'a t -> block Link.t -> unit
  val mem : 'a t -> block Link.t -> bool
  val successors : 'a t -> block Link.t -> 'a list
end = struct
  type 'a key = {parent: 'a -> block Link.t; this: 'a -> block Link.t}

  type 'a t =
    { key: 'a key
    ; by_prnt: (block Link.t, 'a) Hashtbl.t
    ; by_hash: (block Link.t, 'a) Hashtbl.t }

  let create key = {key; by_prnt= Hashtbl.create 5; by_hash= Hashtbl.create 5}
  let get t = Hashtbl.find_opt t.by_hash

  let rm t lnk =
    match get t lnk with
    | None -> ()
    | Some e ->
        Hashtbl.remove t.by_hash lnk ;
        let key = t.key.parent e in
        Hashtbl.find_all t.by_prnt key
        |> List.iter (fun _ -> Hashtbl.remove t.by_prnt key)

  let add t e =
    Hashtbl.add t.by_prnt (t.key.parent e) e ;
    Hashtbl.replace t.by_hash (t.key.this e) e

  let mem t = Hashtbl.mem t.by_hash
  let successors t = Hashtbl.find_all t.by_prnt
end

module Chain : sig
  (** Blockchain. TODO: Fix interface documentation *)
  type t

  val count_vote : t -> vote -> bool (* Returns true if vote is fresh *)

  val add_block : t -> block Link.t -> block -> unit
  (** [add t block] adds [block] to store [t]. *)

  val create : Poll.t -> t
  (** [create ()] creates an empty block store. *)

  val mem : t -> block Link.t -> bool
  val read_state : t -> App.state
  val head : t -> block
  val head_lnk : t -> block Link.t
end = struct
  type detached = {b: block; received_at: int; hash: block Link.t}

  type attached =
    { b: block
    ; height: int
    ; s: App.state
    ; hash: block Link.t
    ; mutable received_detached: block Link.t list
          (* detached blocks received during the time when block was head *) }

  type t =
    { mutable head: attached
    ; mutable truth: attached
    ; detached: detached BlockStore.t
    ; attached: attached BlockStore.t
    ; poll: Poll.t (* TODO: votes could be stored in the same indexed ds *) }

  open BlockStore

  let leader_weight block =
    let id, s = List.nth block.quorum 0 in
    Weight.weigh (block.parent, id, s)

  let read_state t = t.truth.s
  let mem t lnk = mem t.attached lnk || mem t.detached lnk

  let update_head t candidate =
    if
      candidate.height > t.head.height
      || candidate.height = t.head.height
         && ( leader_weight candidate.b < leader_weight t.head.b
            || leader_weight candidate.b = leader_weight t.head.b
               && Poll.(
                    progress t.poll candidate.hash
                    > progress t.poll t.head.hash) )
    then (
      let parent (a : attached) =
        match get t.attached a.b.parent with
        | Some a -> a
        | None when t.truth.height > 0 ->
            raise (Failure "Premature garbage collection")
        | None -> raise Not_found
      in
      (* Update head and commit *)
      t.head <- candidate ;
      let old_truth = t.truth in
      match parent (parent (parent t.head)) with
      | new_truth when old_truth <> new_truth ->
          (* drop old truth *)
          rm t.attached old_truth.hash ;
          (* drop all alternatives to new truth *)
          List.iter
            (fun a -> if a.hash <> new_truth.hash then rm t.attached a.hash)
            (successors t.attached old_truth.hash) ;
          (* drop all detached blocks received with old truth. *)
          List.iter (rm t.detached) old_truth.received_detached ;
          (* garbage collect votes TODO: avoid iteration *)
          Poll.gc ~keep:(mem t) t.poll ;
          (* set new truth *)
          t.truth <- new_truth
      | _ -> ()
      | exception Not_found -> () )

  let rec attach t (to_ : attached) =
    successors t.detached to_.hash
    |> List.iter (fun (d : detached) ->
           let () = rm t.detached d.hash in
           let quorum = d.b.quorum
           and parent = d.b.parent
           and hash = d.hash
           and b = d.b in
           let a : attached =
             { b
             ; height= to_.height + 1
             ; s= App.apply ~hash ~parent ~quorum b.body to_.s
             ; hash
             ; received_detached= [] }
           in
           add t.attached a ; update_head t a ; attach t a )

  let add_block t hash (b : block) =
    match get t.attached b.parent with
    | Some e ->
        let quorum = b.quorum and parent = b.parent in
        let a =
          { b
          ; height= e.height + 1
          ; s= App.apply ~hash ~parent ~quorum b.body e.s
          ; hash
          ; received_detached= [] }
        in
        add t.attached a ; update_head t a ; attach t a
    | None ->
        add t.detached {b; hash; received_at= t.head.height} ;
        t.head.received_detached <- hash :: t.head.received_detached

  let count_vote t ((lnk, _, _) as vote) =
    let fresh = Poll.count t.poll vote in
    let () =
      match get t.attached lnk with Some a -> update_head t a | None -> ()
    in
    fresh

  let head t = t.head.b
  let head_lnk t = t.head.hash

  let create poll =
    let hash = Link.hash genesis in
    let attached =
      create
        { parent= (fun (a : attached) -> a.b.parent)
        ; this= (fun (a : attached) -> a.hash) }
    and detached =
      create
        { parent= (fun (a : detached) -> a.b.parent)
        ; this= (fun (a : detached) -> a.hash) }
    and head =
      {height= 0; b= genesis; s= App.initial; hash; received_detached= []}
    in
    add attached head ;
    {attached; head; detached; poll; truth= head}
end

module Spawn (Broadcast : Broadcast) (Config : Config) : Node = struct
  open Config

  let q = {size= quorum_size; threshold= quorum_threshold}
  let poll = Poll.create ~q my_id
  let chain = Chain.create poll

  (* TODO: Avoid optional/named arguments in visible parts *)
  let propose ~replace lnk =
    match Poll.lead ~replace poll lnk with
    | Some quorum ->
        (* {{{ *)
        assert (valid_quorum q lnk quorum) ;
        (* }}} *)
        let block =
          let body = App.propose () in
          block ~lnk ~quorum ~body my_secret
        in
        (* {{{ *)
        assert (valid_block q block) ;
        (* }}} *)
        let before = Chain.head_lnk chain and hash = Link.hash block in
        Chain.add_block chain hash block ;
        (* Is the new block better than the old one? Then share! *)
        if before <> Chain.head_lnk chain then (
          Broadcast.send (Block block) ;
          true )
        else false
    | None -> false

  let on_receive = function
    | Vote ((lnk, _, _) as vote) when Weight.weigh vote <= vote_threshold ->
        let better = Chain.count_vote chain vote in
        (* Propose using the new vote *)
        ignore (propose ~replace:true lnk) ;
        better
    | Vote _ -> false
    | Block block ->
        let hash = Link.hash block in
        if Chain.mem chain hash then false
        else (
          (* Read votes from quorum *)
          List.iter
            (fun (id, s) ->
              ignore (Chain.count_vote chain (block.parent, id, s)) )
            block.quorum ;
          let valid = valid_block (* {{{ *) q (* }}} *) block in
          (* Consider adding the block *)
          if valid_block (* {{{ *) q (* }}} *) block then
            Chain.add_block chain hash block ;
          (* Attempt to replace dishonest leader *)
          if propose ~replace:false block.parent then false else valid )

  (* The simulator triggers this event and sets trivial thresholds. *)
  let on_atv s =
    let lnk = Chain.head_lnk chain in
    let vote = (lnk, my_id, s) in
    (* {{{ *)
    assert (Weight.weigh vote <= vote_threshold) ;
    (* }}} *)
    ignore (Chain.count_vote chain vote) ;
    if not (propose ~replace:true lnk) then Broadcast.send (Vote vote)

  let work () =
    let i = ref 0 in
    while true do
      incr i ;
      if Weight.weigh (Chain.head_lnk chain, my_id, !i) <= vote_threshold then
        on_atv !i
    done

  (* {{{ *)

  let get_state () = Chain.read_state chain
end

(* }}} *)
