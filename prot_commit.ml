open Primitives
include Prot_parallel

(** Mutable state for the HotPoW protocol. Implements a receive window for
    blocks. Maintains Application state.
    TODO: This duplicates a lot of stuff in Prot_parallel.Chain. Deduplicate. Only new aspect is commit new rule.
*)
module Chain : sig
  type t

  val count_vote : t -> vote -> bool
  (** Returns true if vote was fresh. *)

  val add_block : t -> block Link.t -> block -> unit
  val create : VoteStore.t -> t
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
    ; votes: VoteStore.t }

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
               && VoteStore.(
                    progress t.votes candidate.hash
                    > progress t.votes t.head.hash) )
    then (
      let parent (a : attached) =
        match get t.attached a.b.parent with
        | Some a -> a
        | None when t.truth.height > 0 ->
            raise (Failure "Premature garbage collection")
        | None -> raise Not_found in
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
          VoteStore.gc ~keep:(mem t) t.votes ;
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
             ; received_detached= [] } in
           add t.attached a ; update_head t a ; attach t a)

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

  let create votes =
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
    {attached; head; detached; votes; truth= head}
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

  let chain = Chain.create votes

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
