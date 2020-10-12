open Intf

(** Dummy reference hash for linked list *)
module Link : Hash = struct
  type 'a t = int

  let hash = Hashtbl.hash
  let equal = ( = )
  let to_string = Printf.sprintf "%x"
end

(** Dummy weight hash for proof of work *)
module Weight : Weight = struct
  (* TODO: investigate whether upper=max_int can cause trouble. Maybe summing
   * the quorum yields overflow? *)
  let upper = 1 lsl 24

  (* Am not sure how big output of Hashtbl.hash may be. Cut it down to 24bit to
     ensure that max_weight is correct. *)
  let weigh x = Hashtbl.hash x mod upper
  let max_weight = upper - 1
end

module DSA = Dummy_dsa

type solution = int
type quorum = (DSA.public_key * solution) list

type block =
  {parent: block Link.t; quorum: quorum; body: payload; signature: block_sig}

and block_sig = (block Link.t * quorum * payload) DSA.signature

and payload = unit

let block ~quorum ~lnk ~body secret =
  { quorum
  ; body
  ; parent= lnk
  ; signature= DSA.signature ~secret (lnk, quorum, body) }

module type Config = sig
  val quorum_size : int
  (** quorum size *)

  val quorum_threshold : int
  (** maximum weight of votes *)

  val confirmations : int
  (** number of confirmations until accepting the payload of a block *)

  val my_id : DSA.public_key
  (** The node's public identifier. *)

  val my_secret : DSA.private_key
  (** The private key corresponding to my_id *)
end

type quorum_config = {size: int; threshold: int}
type vote = block Link.t * DSA.public_key * solution
type message = Block of block | Vote of vote

let block_to_string b =
  let open Link in
  Printf.sprintf "Block %s->%s" (hash b |> to_string) (to_string b.parent)

let vote_to_string (lnk, _, _) =
  Printf.sprintf "Vote for %s" (Link.to_string lnk)

let message_to_string = function
  | Block b -> block_to_string b
  | Vote v -> vote_to_string v

module App = struct
  (** This dummy App keeps track of the block height and
      records the history of quorums. *)

  type transition = payload
  type entry = {quorum: quorum; parent: block Link.t; hash: block Link.t}
  type state = {height: int; entries: entry list}

  let initial : state = {height= 0; entries= []}

  let apply ~hash ~quorum ~parent : transition -> state -> state =
   fun _payload s ->
    {height= s.height + 1; entries= {quorum; parent; hash} :: s.entries}

  let propose () : transition = ()
  let verify _ = true
end

module type Broadcast = Broadcast with type message := message
module type Node = Node with type message := message and type state := App.state

module type Implementation = sig
  module Spawn (B : Broadcast) (C : Config) : Node
end
