(* Attempt to speed up simulation by:
 * - avoid hash functions
 * - maintain hash-induced vote order centrally
 * - deduplicate state where possible
 *
 * hash of vote/block -> integer id
 * hashtable of blocks -> ring buffer
 * node-local state -> global data structure w/ bitarray
 *)

module Q = Event_queue

module Bitarray = struct
  type t = { len : int; buf : bytes }

  let create len x =
    let init = (if x = true then '\255' else '\000') in
    let buf = Bytes.make (len / 8 + 1) init in
    { len = len; buf = buf }

  let get t i =
    let ch = int_of_char (Bytes.get t.buf (i lsr 3)) in
    let mask = 1 lsl (i land 7) in
    (ch land mask) <> 0

  let set t i b =
    let index = i lsr 3 in
    let ch = int_of_char (Bytes.get t.buf (index)) in
    let mask = 1 lsl (i land 7) in
    let new_ch = if b then (ch lor mask) else (ch land lnot mask) in
    Bytes.set t.buf index (char_of_int new_ch)
end

type vote =
  { vid : int
  ; parent: int (* referenced block *)
  ; weight : int (* H(vote) *)
  ; node: int (* authorized node *)
  ; known: Bitarray.t (* which nodes are aware of this vote? *)
  }

module VoteSet = struct
  module S = Set.Make (struct
      type t = vote

      let compare a b = compare (a.weight, a.vid) (b.weight, b.vid)
    end)

  type t = S.t ref

  let create () = ref S.empty
end

type block =
  { (* static: the block itself *)
    bid : int
  ; parent : int
  ; quorum: vote list
  ; node: int (* proposing node *)
  ; (* mutable: simulator state associated with this block *)
    votes: VoteSet.t (* votes for this block *)
  ; known: Bitarray.t (* which nodes are aware of this block? *)
  }

type event =
  | Activation
  | Deliver_vote of {vote: vote; rcv: int}
  | Deliver_block of {block: block; rcv: int}

type node =
  { mutable head : int }

type world =
  { n_nodes: int
  ; nodes : node array
  ; blocks: block array (* block ring buffer *)
  ; votes: vote array (* vote ring buffer *)
  ; mutable last_vote: int
  ; mutable last_block: int
  ; mutable time: float
  ; mutable queue: event Q.t
  ; mutable shutdown: bool (* shut down the simulation *)
  }

let init ~nodes ~buffer_size =
  let v =
    { vid = -1
    ; parent = -1
    ; weight = max_int
    ; node = -1
    ; known = Bitarray.create nodes false
    }
  and b =
    { bid = -1
    ; parent = -1
    ; quorum = []
    ; node = -1
    ; votes = VoteSet.create ()
    ; known = Bitarray.create nodes false
    }
  in
  { n_nodes = nodes
  ; last_vote = -1
  ; last_block = -1
  ; nodes = Array.make nodes {head = -1}
  ; votes = Array.make (fst buffer_size) v
  ; blocks = Array.make (snd buffer_size) b
  ; time = 0.
  ; queue = Q.(schedule empty 0. Activation)
  ; shutdown = false
  }

let schedule ~delta world event =
  world.queue <- Q.schedule world.queue (world.time +. delta) event

module Rv = struct
  type distribution =
    | Uniform
    | Exponential

  let draw ~ev =
    function
    | Uniform -> ev *. Random.float 2.
    | Exponential -> -1. *. ev *. log (Random.float 1.)
end

let vid world =
  let vid = world.last_vote + 1 in
  world.last_vote <- vid; vid

let bid world =
  let bid = world.last_block + 1 in
  world.last_block <- bid; bid

let activate world node =
  let known = Bitarray.create world.n_nodes false in
  let () = Bitarray.set known node true in
  let vote =
    { vid = vid world
    ; parent = world.nodes.(node).head
    ; weight = Random.bits ()
    ; node
    ; known
    }
  in
  (* Protocol.activation world.nodes.(node) vote *)
  ignore (vote)

let handle_event world event =
  match event with
  | Activation ->
    let () = (* schedule next activation *)
      if not world.shutdown then
        schedule ~delta:Rv.(draw ~ev:1. Exponential) world Activation
    in
    let node = (* sample node *) Random.int world.n_nodes in
    activate world node
  | Deliver_vote _v -> ()
  (* Protocol.rcv_vote world.nodes.(v.rcv) v.vote *)
  | Deliver_block _ -> ()
  (* Protocol.rcv_block world.nodes.(v.rcv) v.block *)
