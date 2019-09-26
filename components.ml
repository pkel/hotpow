module type Application = sig
  type transition
  type state

  val initial : state
  val apply : transition -> state -> state
  val propose : unit -> transition
  val verify : transition -> bool
end

module type DSA = sig
  type public_key
  type private_key
  type 'a signature

  val signature : secret:private_key -> 'a -> 'a signature
  val verify : id:public_key -> 'a -> 'a signature -> bool
  val generate_id : unit -> public_key * private_key
  val string_of_id : public_key -> string
  val int_of_id : public_key -> int
end

module type Hash = sig
  type 'a t

  val hash : 'a -> 'a t
  val equal : 'a t -> 'a t -> bool
  val to_string : 'a t -> string
end

module type Weight = sig
  val weigh : 'a -> int
  val max_weight : int
end

module type Broadcast = sig
  type message

  val send : message -> unit
end

module type Node = sig
  type message
  type state

  val on_receive : message -> unit

  val work : unit -> unit
  (** Search ATVs with proof-of-work *)

  val on_atv : int -> unit
  (** Handle the nth ATV. Will only work when PoW threshold is trivial. *)

  val get_state : unit -> state
  (** Extract committed application state. *)
end
