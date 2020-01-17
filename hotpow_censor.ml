(* Protocol Deviation: Censoring

   The censoring attacker wants to contribute as many blocks to the blockchain
   as possible. He proposes complete blocks but withholds all of his votes.  *)

open Primitives
open Hotpow

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
          let body = App.propose () in
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
        (* Propose using the new vote.
           Strategy: only propose when we are the truthful leader. Otherwise
           we would leak our withheld votes. *)
        if fresh then ignore (propose ~replace:false lnk) ;
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
    (* Strategy: withhold all votes; propose full blocks *)
    ignore (propose ~replace:false lnk)

  let work () =
    let i = ref 0 in
    while true do
      incr i ;
      if Weight.weigh (Chain.head_lnk chain, my_id, !i) <= config.threshold then
        on_atv !i
    done

  let get_state () = Chain.read_state chain
end
