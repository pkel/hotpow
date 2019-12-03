open Primitives

type t = Naive | Censor | Censor_simple | Selfish

let to_string = function
  | Naive -> "naive"
  | Censor -> "censor"
  | Censor_simple -> "censor-simple"
  | Selfish -> "selfish"

let of_string = function
  | "naive" -> Naive
  | "censor" -> Censor
  | "censor-simple" -> Censor_simple
  | "selfish" -> Selfish
  | s -> failwith ("unkown strategy: " ^ s)

let to_implementation : t -> (module Implementation) = function
  | Naive -> (module Hotpow)
  | Censor -> (module Hotpow_censor)
  | Censor_simple -> (module Hotpow_censor_simple)
  | Selfish -> (module Hotpow_selfish)

let enum =
  List.map (fun s -> (to_string s, s)) [Naive; Censor; Censor_simple; Selfish]
