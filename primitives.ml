open Intf

(** Dummy reference hash for linked list *)
module Link : Hash = struct
  type 'a t = int

  let hash x =
    (* Ocaml's Murmur3 has 32 bit state. It returns a non-negative Ocaml int, so
       effectively 30 bits. That's not enough for our experiments.

       Probability of hash collision in K runs over k blocks and l bits: p = 1 -
       (1 - k * (k-1) / 2 / 2^l)^K K = 4800 k = 1024 l = 30 -> p = 0.90 l = 60
       -> p = 2.18e-09

       We should be fine for now with two hashes or 60 bit. *)
    let a = Hashtbl.seeded_hash_param 255 255 42 x
    and b = Hashtbl.seeded_hash_param 255 255 0 x in
    (a lsl 30) + b

  let equal = ( = )
  let to_string = Printf.sprintf "0x%015x"
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

and payload = {time: float}

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

  val now : unit -> float
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
  (** This dummy App keeps track of the block height and records the history of
      quorums. *)

  type transition = payload

  type entry =
    { height: int
    ; quorum: quorum
    ; parent: block Link.t
    ; hash: block Link.t
    ; payload: transition }

  type state = entry list

  let initial : state = []

  let apply ~hash ~quorum ~parent : transition -> state -> state =
   fun payload s ->
    let height = match s with [] -> 0 | {height; _} :: _ -> height + 1 in
    {height; quorum; parent; hash; payload} :: s

  (* TODO: tracking time in the app is redundant since the simulator build a
     block tree itself. remove *)
  let propose ~time : transition = {time}
  let verify _ = true
end

module type Broadcast = Broadcast with type message := message
module type Node = Node with type message := message and type state := App.state

module type Implementation = sig
  module Spawn (B : Broadcast) (C : Config) : Node
end
