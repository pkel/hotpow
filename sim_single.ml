open Simulator

include struct
  [@@@ocaml.warning "-39"]

  type cli =
    { header: bool  (** Print csv headers and exit. *)
    ; block_file: string option [@aka ["o"]]
          (** Write per-block measurements to the given file. Includes only the
              longest chain up to the confirmed height (see --confirmations) *)
    }
  [@@deriving cmdliner]
end

let term =
  let f cli io p =
    let () =
      match check_params p with
      | Error m ->
          Printf.eprintf "Invalid parameter: %s\n" m ;
          exit 1
      | Ok _ -> () in
    if cli.header then (
      print_endline (csv_head cols) ;
      exit 0 ) ;
    let r = simulate io p in
    let () =
      match cli.block_file with
      | Some fname ->
          let f = open_out fname in
          Printf.fprintf f "%s\n" (csv_head block_cols) ;
          List.iter
            (fun b -> Printf.fprintf f "%s\n" (csv_row block_cols b))
            r.blocks ;
          close_out f
      | None -> () in
    let row = {p; r} in
    print_endline (csv_row cols row) in
  Cmdliner.Term.(
    const f $ cli_cmdliner_term () $ io_params_cmdliner_term ()
    $ params_cmdliner_term ())

let info = Cmdliner.(Term.info "powsim" ~doc:"Proof-of-Work Simulator")
let () = Cmdliner.(Term.exit @@ Term.eval (term, info))
