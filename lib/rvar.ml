open Sexplib.Std

type _ t =
  | Constant : 'a -> 'a t
  | Uniform : {lower: float; delta: float} -> float t
  | Exponential : {neg_ev: float} -> float t
  | Discrete :
      {p: float array; alias: int option array; el: 'a array; n: int}
      -> 'a t

let uniform ~lower ~upper =
  if lower < upper then `Ok (Uniform {lower; delta= upper -. lower})
  else `Invalid_parameters "lower >= upper"

let constant c = Constant c

let exponential' ~rate =
  if rate > 0. then `Ok (Exponential {neg_ev= -1. /. rate})
  else `Invalid_parameters "rate <= 0"

let exponential ~ev =
  if ev > 0. then `Ok (Exponential {neg_ev= Float.neg ev})
  else `Invalid_parameters "ev <= 0"

(* Voses Alias Method for efficient sampling of discrete random variables
   http://keithschwarz.com/darts-dice-coins/
   https://alaska-kamtchatka.blogspot.com/2011/12/voses-alias-method.html

   TODO: Manual tests show that it works. Proper evaluation is missing!
*)
let discrete =
  let init ~n ~el ~p ~alias =
    let rec f = function
      | (ps, is) :: small, (pl, il) :: large ->
          p.(is) <- ps ;
          alias.(is) <- Some il ;
          let p' = ps +. pl -. 1. in
          if p' < 1. then f ((p', il) :: small, large)
          else f (small, (p', il) :: large)
      | (_p, i) :: small, [] ->
          p.(i) <- 1. ;
          alias.(i) <- None ;
          f (small, [])
      | [], (_p, i) :: large ->
          p.(i) <- 1. ;
          alias.(i) <- None ;
          f ([], large)
      | [], [] -> Discrete {el; p; alias; n} in
    f in
  fun l ->
    let sum, n, err =
      List.fold_left
        (fun (sum, i, err) (p, _) ->
          if p < 0. then (sum, i, Some "negative probability")
          else (sum +. p, i + 1, err))
        (0., 0, None) l in
    match err with
    | Some err -> `Invalid_parameters err
    | None when n < 1 -> `Invalid_parameters "empty list"
    | None ->
        let el =
          let r = ref l in
          Array.init n (fun _i ->
              let e = snd (List.hd !r) in
              r := List.tl !r ;
              e)
        and p = Array.make n 0.
        and alias = Array.make n None
        and small, large, _ =
          let scale = float_of_int n /. sum in
          List.fold_left
            (fun (s, l, i) (p, _) ->
              let p = p *. scale in
              if p < 1. then ((p, i) :: s, l, i + 1) else (s, (p, i) :: l, i + 1))
            ([], [], 0) l in
        `Ok (init ~el ~n ~p ~alias (small, large))

let sample : type a. a t -> a = function
  | Constant c -> c
  | Uniform p -> p.lower +. Random.float p.delta
  | Exponential p -> p.neg_ev *. log (Random.float 1.)
  | Discrete {p; alias; el; n} -> (
      let i = Random.int n in
      match alias.(i) with
      | None -> el.(i)
      | Some j -> if Random.float 1. > p.(i) then el.(j) else el.(i) )

type stringable =
  | Constant of float
  | Uniform of float * float
  | Exponential of float  (** Expected Value *)
[@@deriving sexp]

let float_to_stringable : float t -> stringable = function
  | Constant c -> Constant c
  | Uniform p -> Uniform (p.lower, p.lower +. p.delta)
  | Exponential p -> Exponential (Float.neg p.neg_ev)
  | Discrete _ ->
      failwith "serialization of discrete distribution not yet implemented"

let float_of_stringable : stringable -> _ = function
  | Constant c -> `Ok (constant c)
  | Uniform (lower, upper) -> uniform ~lower ~upper
  | Exponential ev -> exponential ~ev

let float_to_string t =
  match float_to_stringable t with
  | Constant f -> string_of_float f
  | t' ->
      let s = sexp_of_stringable t' |> Sexplib.Sexp.to_string in
      String.sub s 1 (String.length s - 2)

let float_of_string s =
  let s = String.trim s in
  match float_of_string_opt s with
  | Some f -> `Ok (constant f)
  | None -> (
      let s = if String.length s = 0 || s.[0] != '(' then "(" ^ s ^ ")" else s in
      try
        Sexplib.Sexp.of_string s |> stringable_of_sexp |> float_of_stringable
      with
      | Sexplib0.Sexp.Of_sexp_error (Failure s, _) | Failure s -> `Parse_error s
      | _ -> `Parse_error "generic parse error" )

let fail = function
  | `Ok x -> x
  | `Invalid_parameters s -> failwith ("invalid parameters: " ^ s)
  | `Parse_error s -> failwith ("parse error: " ^ s)
