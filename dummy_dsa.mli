include Intf.DSA

module Lookup : sig
  type 'a t

  val create : unit -> 'a t
  val add : public_key -> 'a -> 'a t -> 'a t
  val get : 'a t -> public_key -> 'a option
  val get_exn : 'a t -> public_key -> 'a
end
