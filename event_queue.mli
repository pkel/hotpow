type time = float
type 'a t

val empty : 'a t
val schedule : 'a t -> time -> 'a -> 'a t

val next : 'a t -> time * 'a * 'a t
(** raise Not_found in case of empty queue *)
