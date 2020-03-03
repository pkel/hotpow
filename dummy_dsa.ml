type 'a signature = string
type public_key = int
type private_key = int

let signature ~secret _msg = "signed by " ^ string_of_int secret
let verify ~id msg sig_ = signature ~secret:id msg = sig_
let count = ref 0

let generate_id () =
  let nr = !count in
  incr count ; (nr, nr)

let string_of_id = string_of_int
let int_of_id x = x

module Lookup = struct
  type 'a t = {assoc: (int * 'a) list; table: 'a option array lazy_t}

  let return assoc =
    let table =
      lazy
        (let a = Array.make !count None in
         List.iter (fun (pkey, el) -> a.(pkey) <- Some el) assoc ;
         a) in
    {assoc; table}

  let create () = return []
  let add pkey el {assoc; _} = return ((pkey, el) :: assoc)
  let get {table= (lazy a); _} pkey = a.(pkey)

  let get_exn t pkey =
    match get t pkey with None -> raise Not_found | Some x -> x
end
