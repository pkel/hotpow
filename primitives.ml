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

type app_meta = {hash: block Link.t; parent: block Link.t; quorum: quorum}

module type Application =
  Application with type meta = app_meta and type transition = unit

module type Broadcast = Broadcast with type message = message
module type Node = Node with type message = message

module type Implementation = sig
  module Spawn (A : Application) (B : Broadcast) (C : Config) :
    Node with type state = A.state
end
