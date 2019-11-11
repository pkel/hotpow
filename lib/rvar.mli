(** {1} Random variables

    This module provides a type for random variables. Random variables can be
    sampled according to their probability distribution.

    This is based on OCaml's {!Random} module. Consider executing
    [Random.self_init ()] or similar before using {!sample}.
*)

(** Random variable *)
type 'a t

val sample : 'a t -> 'a
(** Sample a random variable. *)

(** {2} Constructors *)

val constant : 'a -> 'a t
(** [sample (constant x)] yields [x]. *)

val uniform :
  lower:float -> upper:float -> [`Ok of float t | `Invalid_parameters of string]
(** Continuous uniform distribution over the given interval. *)

val exponential : ev:float -> [`Ok of float t | `Invalid_parameters of string]
(** Exponential distribution parametrized by expected value. *)

val exponential' :
  rate:float -> [`Ok of float t | `Invalid_parameters of string]
(** Exponential distribution parametrized by rate. *)

val discrete :
  (float * 'a) list -> [`Ok of 'a t | `Invalid_parameters of string]
(** Generic discrete distribution. Takes a list of tuples (prob, outcome) as
    income. Probabilities not summing up to 1 will be automatically scaled.

    We use the Alias Method to achieve constant sampling time and linear (number
    of elements) initialization time.
*)

(** {2} Serialization

    WIP. Currently only supports float.  Will throws {!Failure} on serializing
    discrete distribution.
*)

val float_to_string : float t -> string

val float_of_string :
     string
  -> [> `Ok of float t | `Invalid_parameters of string | `Parse_error of string]

(** {2} Misc *)

(* Return random variable of raise {!Failure}. *)
val fail :
     [< `Ok of 'a t | `Invalid_parameters of string | `Parse_error of string]
  -> 'a t
