open Primitives

(** Pure helper module for managing a single partial quorum. *)
module PartialQuorum : sig
  type t

  val create : id:DSA.public_key -> config:quorum_config -> block Link.t -> t
  (** [create ~id ~config ref] creates an empty partial quorum for [ref]. *)

  val add : vote -> t -> bool * t
  (** [add v t] adds the vote [v] to the partial quorum [t]. The returned
      boolean indicates, whether the vote was new. *)

  val progress : t -> int
  (** [progress t] counts the number of votes in the partial quorum. *)

  val complete : replace:bool -> t -> quorum option
  (** [complete ?replace t] attempts to return a complete quorum for the id and
      reference given to {create}. This function generally prefers own votes
      over foreign votes. If replace is true, the function attempts to replace
      the truthful leader by omitting small foreign votes. *)
end = struct
  module Elt = struct
    type t = {size: int; prefer: bool; id: DSA.public_key; sol: solution}

    let compare a b =
      let w = Int.compare a.size b.size in
      if w <> 0 then w
      else
        let p = compare a.prefer b.prefer in
        if p <> 0 then p else compare a b
  end

  module Set = Set.Make (Elt)
  open Elt

  type t =
    { for_block: block Link.t
    ; me: DSA.public_key
    ; threshold: int
    ; quorum_size: int
    ; own: Set.t
    ; foreign: Set.t }

  let create ~id ~(config : quorum_config) for_block =
    { for_block
    ; threshold= config.threshold
    ; quorum_size= config.size
    ; me= id
    ; own= Set.empty
    ; foreign= Set.empty }

  let add ((ref, id, sol) as vote) t =
    if ref <> t.for_block then failwith "invalid ref"
    else
      let size = Weight.weigh vote in
      if Int.compare size t.threshold > 0 then (false, t)
      else
        let mine = id = t.me in
        let elt = {size; id; sol; prefer= mine} in
        if mine then
          if Set.mem elt t.own then (false, t)
          else (true, {t with own= Set.add elt t.own})
        else if Set.mem elt t.foreign then (false, t)
        else (true, {t with foreign= Set.add elt t.foreign})

  let complete ~replace t =
    (* TODO: derive feasibility of quorum from (cached) cardinals of the set *)
    match Set.min_elt_opt t.own with
    | None -> None
    | Some my_best -> (
        (* drop small votes of other nodes in order to replace leader*)
        let foreign =
          if replace then
            let _, _, above = Set.split my_best t.foreign in
            above
          else t.foreign in
        (* merge foreign votes into set of own votes in order to complete quorum *)
        let merged =
          let rec f n acc seq =
            if n >= t.quorum_size then acc
            else
              match seq () with
              | Seq.Nil -> acc
              | Cons (elt, tl) -> f (n + 1) (Set.add elt acc) tl in
          f (Set.cardinal t.own) t.own Set.(to_seq foreign) in
        (* consume merged votes into quorum *)
        let quorum =
          let rec f n acc seq =
            if n = t.quorum_size then Some (List.rev acc)
            else
              match seq () with
              | Seq.Nil -> None
              | Cons (elt, tl) -> f (n + 1) ((elt.id, elt.sol) :: acc) tl in
          f 0 [] (Set.to_seq merged) in
        (* for replace=false we have to make sure that we lead TODO: we could
           check this before assembling the quorum. *)
        if replace then quorum
        else
          match quorum with
          | None -> None
          | Some ((id, _) :: _ as q) -> if id = t.me then Some q else None
          | Some _l -> raise (Failure "produced empty quorum") )

  let progress t = Set.cardinal t.own + Set.cardinal t.foreign
end

(** Impure module for managing multiple partial quorums indexed by block
    reference. This is a thin wrapper around the functions exposed by
    {PartialQuorum}. *)
module VoteStore : sig
  type t

  val count : t -> vote -> bool
  (** See {PartialQuorum.add}. *)

  val quorum : replace:bool -> t -> block Link.t -> quorum option
  (** See {PartialQuorum.complete}. *)

  val progress : t -> block Link.t -> int
  (** See {PartialQuorum.progress}. *)

  val gc : keep:(block Link.t -> bool) -> t -> unit
  (** Garbage collection. [gc ~keep t] drops all votes for references from [t],
      for which [keep lnk] yields [false]. *)

  val create : id:DSA.public_key -> config:quorum_config -> t
  (** [create ~id ~config] sets up a vote store that produces quorums for the
      given id. Also see {PartialQuorum.create}. *)
end = struct
  type t =
    { config: quorum_config
    ; id: DSA.public_key
    ; ht: (block Link.t, PartialQuorum.t) Hashtbl.t }

  let create ~id ~config = {config; ht= Hashtbl.create 5; id}

  let count t ((ref, _, _) as vote) =
    let config = t.config and id = t.id in
    let votes =
      match Hashtbl.find_opt t.ht ref with
      | None -> PartialQuorum.create ~id ~config ref
      | Some x -> x in
    let freshness, votes = PartialQuorum.add vote votes in
    Hashtbl.replace t.ht ref votes ;
    freshness

  let gc ~keep t =
    Hashtbl.filter_map_inplace (fun k v -> if keep k then Some v else None) t.ht

  let progress t ref =
    match Hashtbl.find_opt t.ht ref with
    | None -> 0
    | Some votes -> PartialQuorum.progress votes

  let quorum ~replace t ref =
    match Hashtbl.find_opt t.ht ref with
    | None -> None
    | Some votes -> PartialQuorum.complete ~replace votes
end

(** Impure module for storing blocks indexed by hash and parent hash. *)
module BlockStore : sig
  type 'a t
  (** An ['a t] stores values of type ['a].*)

  type 'a key = {parent: 'a -> block Link.t; this: 'a -> block Link.t}
  (** An ['a key] provides functions to read the ['a]'s block hash and parent
      hash. *)

  val create : 'a key -> 'a t
  (** [create key] sets up an empty store. Elements will be indexed using the
      functions in key. *)

  val get : 'a t -> block Link.t -> 'a option
  (** [get t ref] returns the block [b] where [key.this b = ref] if available. *)

  val add : 'a t -> 'a -> unit
  (** Add a block to the store *)

  val rm : 'a t -> block Link.t -> unit
  (** [rm t ref] removes the block [b] with [key.this b = ref]. *)

  val mem : 'a t -> block Link.t -> bool
  (** [mem t ref] returns true iif [get t ref] returns [Some _]. *)

  val successors : 'a t -> block Link.t -> 'a list
  (** [successors t ref] returns all blocks [b] where [key.parent b = ref]. *)

  val elements : 'a t -> 'a list
  (** [elements t] returns all block in the store in unspecified order. *)

  val gc : keep:('a -> bool) -> 'a t -> unit
  (** Garbage collection. [gc ~keep t] drops all blocks [b] from [t], for which
      [keep b] yields [false]. *)
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
    if Hashtbl.mem t.by_hash (t.key.this e) then ()
    else (
      Hashtbl.add t.by_prnt (t.key.parent e) e ;
      Hashtbl.add t.by_hash (t.key.this e) e )

  let mem t = Hashtbl.mem t.by_hash
  let successors t = Hashtbl.find_all t.by_prnt
  let elements t = Hashtbl.fold (fun _k v acc -> v :: acc) t.by_hash []

  let gc ~keep t =
    Hashtbl.fold (fun h b acc -> if keep b then acc else h :: acc) t.by_hash []
    |> List.iter (rm t)
end

let valid_quorum ~config ref quorum =
  let quorum =
    List.map (fun (id, s) -> (id, s, Weight.weigh (ref, id, s))) quorum in
  let rec is_sorted_and_duplicate_free = function
    | [] | [_] -> true
    | (id, s, w) :: (id', s', w') :: _
      when compare w w' > 0 || (id, s) = (id', s') ->
        false
    | _ :: tl -> is_sorted_and_duplicate_free tl in
  List.length quorum == config.size
  && List.for_all (fun (_, _, w) -> compare config.threshold w >= 0) quorum
  && is_sorted_and_duplicate_free quorum

let valid_block ~config (block : block) =
  valid_quorum ~config block.parent block.quorum
  && DSA.verify
       ~id:(fst (List.nth block.quorum 0))
       (block.parent, block.quorum, block.body)
       block.signature

(** (invalid) root block *)
let genesis : block =
  let parent = Obj.magic Link.hash "genesis block"
  and quorum = [(Obj.magic 0, 0)] in
  { parent
  ; quorum
  ; body= {time= 0.} (* This transition will not be executed *)
  ; signature= Obj.magic "signed by Satoshi" }

(** Mutable state for the parallel PoW protocol. Implements a receive window for
    blocks. Maintains application state. TODO: abstract commit rule and reuse
    this for HotPoW. Maybe have one module that implements block tree and
    receive window. *)
module Chain : sig
  type t

  val count_vote : t -> vote -> bool
  (** Returns true if vote was fresh. *)

  val add_block : t -> block Link.t -> block -> unit
  val create : int -> VoteStore.t -> t
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
    ; votes: VoteStore.t
    ; confirmations: int }

  open BlockStore

  let read_state t = t.truth.s
  let mem t lnk = mem t.attached lnk || mem t.detached lnk

  let update_head t candidate =
    (* longest chain rule block > most votes rule *)
    if
      candidate.height > t.head.height
      || candidate.height = t.head.height
         && VoteStore.(
              progress t.votes candidate.hash > progress t.votes t.head.hash)
    then
      let parent' (a : attached) =
        match get t.attached a.b.parent with
        | Some a -> a
        | None when t.truth.height > 0 ->
            raise (Failure "Premature garbage collection")
        | None -> raise Not_found in
      let rec parent n a = if n < 1 then a else parent (n - 1) (parent' a) in
      (* Update head and application state *)
      let () =
        t.head <- candidate ;
        try
          let new_truth = parent t.confirmations t.head in
          if
            not
              Link.(
                equal t.truth.hash (parent 1 new_truth).hash
                || equal t.truth.hash new_truth.hash)
          then
            Printf.eprintf
              "WARNING: inconsistency detected (head.height: %i <- %i) \
               (truth.height: %i <- %i)\n\
               %!"
              candidate.height t.head.height t.truth.height new_truth.height ;
          (* TODO: count inconsistencies *)
          t.truth <- new_truth
        with Not_found -> () in
      (* garbage collect old attached blocks *)
      if t.head.height mod 100 = 0 then (
        let cutoff = t.head.height - 100 in
        gc ~keep:(fun b -> b.height > cutoff) t.attached ;
        gc ~keep:(fun b -> b.received_at > cutoff) t.detached ;
        VoteStore.gc ~keep:(mem t) t.votes )

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
             ; received_detached= [] } in
           add t.attached a ; update_head t a ; attach t a)

  (* TODO: rewrite add/attach logic such that incoming blocks are added to
   * detached block store on income. Then attach.
   * Other idea: share block store for attached/detached blocks. *)

  let add_block t hash (b : block) =
    match get t.attached b.parent with
    | Some e ->
        let quorum = b.quorum and parent = b.parent in
        let a =
          { b
          ; height= e.height + 1
          ; s= App.apply ~hash ~parent ~quorum b.body e.s
          ; hash
          ; received_detached= [] } in
        add t.attached a ; update_head t a ; attach t a
    | None ->
        add t.detached {b; hash; received_at= t.head.height} ;
        t.head.received_detached <- hash :: t.head.received_detached

  let count_vote t ((lnk, _, _) as vote) =
    let fresh = VoteStore.count t.votes vote in
    let () =
      match get t.attached lnk with Some a -> update_head t a | None -> () in
    fresh

  let head t = t.head.b
  let head_lnk t = t.head.hash

  let create confirmations votes =
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
      {height= 0; b= genesis; s= App.initial; hash; received_detached= []} in
    add attached head ;
    {attached; head; detached; votes; truth= head; confirmations}
end

module Spawn (Broadcast : Broadcast) (Config : Config) : Node = struct
  open Config

  let config =
    let open Config in
    {size= quorum_size; threshold= quorum_threshold}

  let votes =
    let config = {size= quorum_size; threshold= quorum_threshold}
    and id = my_id in
    VoteStore.create ~id ~config

  let chain = Chain.create Config.confirmations votes

  let propose ~replace lnk =
    match VoteStore.quorum ~replace votes lnk with
    | Some quorum ->
        assert (valid_quorum ~config lnk quorum) ;
        let block =
          let body = App.propose ~time:(Config.now ()) in
          block ~lnk ~quorum ~body my_secret in
        assert (valid_block ~config block) ;
        let before = Chain.head_lnk chain and hash = Link.hash block in
        Chain.add_block chain hash block ;
        (* Is the new block better than the old one? Then share! *)
        if before <> Chain.head_lnk chain then (
          Broadcast.send (Block block) ;
          true )
        else false
    | None -> false

  let on_receive = function
    | Vote ((lnk, _, _) as vote) when Weight.weigh vote <= quorum_threshold ->
        let fresh = Chain.count_vote chain vote in
        (* Propose using the new vote. *)
        if fresh then ignore (propose ~replace:true lnk) ;
        fresh
    | Vote _ -> false
    | Block block ->
        let hash = Link.hash block in
        if Chain.mem chain hash then false
        else (
          (* Read votes from quorum *)
          List.iter
            (fun (id, s) ->
              ignore (Chain.count_vote chain (block.parent, id, s)))
            block.quorum ;
          (* Add block if valid *)
          let valid = valid_block ~config block in
          if valid then Chain.add_block chain hash block ;
          (* Attempt to replace dishonest leader *)
          if propose ~replace:false block.parent then false else valid )

  (* The simulator triggers this event and sets trivial thresholds. *)
  let on_atv s =
    let lnk = Chain.head_lnk chain in
    let vote = (lnk, my_id, s) in
    assert (Weight.weigh vote <= config.threshold) ;
    ignore (Chain.count_vote chain vote) ;
    if not (propose ~replace:true lnk) then Broadcast.send (Vote vote)

  let work () =
    let i = ref 0 in
    while true do
      incr i ;
      if Weight.weigh (Chain.head_lnk chain, my_id, !i) <= config.threshold then
        on_atv !i
    done

  let get_state () = Chain.read_state chain
end
