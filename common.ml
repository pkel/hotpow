(* {{{ *)

open Components

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

(* }}} *)

type quorum = (DSA.public_key * solution) list

type block =
  {parent: block Link.t; quorum: quorum; body: payload; signature: block_sig}

(* {{{ *)
and block_sig = (block Link.t * quorum * payload) DSA.signature

and payload = unit

let block ~quorum ~lnk ~body secret =
  { quorum
  ; body
  ; parent= lnk
  ; signature= DSA.signature ~secret (lnk, quorum, body) }

module type Config = sig
  val quorum_threshold : int
  (** quorum threshold *)

  val quorum_size : int
  (** quorum size *)

  val vote_threshold : int
  (** maximum weight of votes *)

  val my_id : DSA.public_key
  (** The node's public identifier. *)

  val my_secret : DSA.private_key
  (** The private key corresponding to my_id *)
end

type quorum_config = {size: int; threshold: int}
type vote = block Link.t * DSA.public_key * solution
type message = Block of block | Vote of vote

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

let is_sorted_and_duplicate_free q lnk =
  let open Weight in
  let rec f = function
    | [] | [_] -> true
    | (id, s) :: (id', s') :: _
      when weigh (lnk, id, s) > weigh (lnk, id', s') || (id, s) = (id', s') ->
        false
    | _ :: (_ :: _ as tl) -> f tl
  in
  f q

(** A quorum certificate is valid if it has the correct size and the weights
    of its votes sum up to a small value. }}} *)
let valid_quorum (* {{{ *) q (* }}} *) lnk quorum =
  (* {{{ *)
  let open Weight in
  let quorum_size = q.size and quorum_threshold = q.threshold in
  (* }}} *)
  let sum_of_weights lst =
    List.fold_left (fun acc (id, s) -> acc + weigh (lnk, id, s)) 0 lst
  in
  List.length quorum == quorum_size
  && is_sorted_and_duplicate_free quorum lnk
  && sum_of_weights quorum <= quorum_threshold * quorum_size

(* {{{ *)

(** A block is valid if we know its parent and its height, quorum certificate
    and signature are valid. It must have been proposed by the producer of the
    smallest vote in the quorum. }}} *)
let valid_block (* {{{ *) q (* }}} *) block =
  valid_quorum (* {{{ *) q (* }}} *) block.parent block.quorum
  && App.verify block.body
  && DSA.verify
       ~id:(fst (List.nth block.quorum 0))
       (block.parent, block.quorum, block.body)
       block.signature

(* {{{ *)

(** (invalid) root block *)
let genesis : block =
  let parent = Obj.magic Link.hash "genesis block"
  and quorum = [(Obj.magic 0, 0)] in
  { parent
  ; quorum
  ; body= () (* This transition will not be executed *)
  ; signature= Obj.magic "signed by Satoshi" }

module type Broadcast = Broadcast with type message := message

module type Node =
  Node with type message := message and type state := App.state

module type Implementation = sig
  module Spawn (B : Broadcast) (C : Config) : Node
end

(* }}} *)
