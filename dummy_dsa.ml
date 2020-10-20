type 'a signature = string
type public_key = int
type private_key = int

let signature ~secret _msg = "signed by " ^ string_of_int secret
let verify ~id msg sig_ = signature ~secret:id msg = sig_
let cnt = ref (-1)
let pre_inc cnt = incr cnt ; !cnt

let generate_id () =
  let nr = pre_inc cnt in
  (nr, nr)

let id_of_int i = (i, i)

let string_of_id = string_of_int
let int_of_id x = x
