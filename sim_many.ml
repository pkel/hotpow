open Base

(* TODO read from command line *)
let n_blocks = 128
let n_nodes = 32
let n_cores = 4

let range a b =
  let rec r acc b = if b < a then acc else r (b :: acc) (b - 1) in
  r [] b

let map f l = List.map ~f l
let foldl f init l = List.fold_left ~f ~init l

let rational a b =
  (Float.of_int a) /. (Float.of_int b)

let estimate_time cfg =
  let open Simulator in
  cfg.quorum_size * cfg.n_blocks * cfg.n_nodes

let check_configs l =
  let ok = ref true in
  List.iteri ~f:(fun i c ->
      let open Stdio in
      match Simulator.check_params c with
      | Ok _ -> ()
      | Error s ->
        eprintf "Error in %i-th configuration: %s\n%s\n%!" i s (Simulator.show_params c);
        ok := false
    ) l;
  if not !ok then Caml.exit 1

let configs =
  range 0 8
  |> map (fun x -> 1 lsl x)
  |> map (fun quorum_size ->
      let open Simulator in
      { n_nodes
      ; n_blocks
      ; protocol = Parallel
      ; quorum_size
      ; confirmations = 3
      ; delta_dist = Exponential
      ; delta_vote = 0.
      ; delta_block = 0.
      ; leader_failure_rate = 0.
      ; churn = 0.
      ; eclipse_time = 10.
      ; (* Attacker *)
        alpha = rational 1 n_nodes
      ; strategy = Parallel
      }
    )

let () =
  let open Stdio in
  check_configs configs;
  printf "Simulating %i configurations...\n%!" (List.length configs);
  let time_estimate =
    List.fold_left ~init:0 ~f:(+) (map estimate_time configs)
  in
  let progress = ref 0 in
  let io = Simulator.{ verbosity=0; progress=false } in
  let runs_file = Out_channel.create "output/runs.csv" in
  Out_channel.fprintf runs_file ("%s\n") Simulator.(csv_head cols);
  let log_run p r =
    Out_channel.fprintf runs_file ("%s\n") Simulator.(csv_row cols {p;r})
  and write_blocks cfg r =
    let open Simulator in
    let h = hash_params cfg in
    let block_file = Out_channel.create (Printf.sprintf "output/%08x-blocks.csv" h) in
    Out_channel.fprintf block_file "%s\n" (csv_head block_cols) ;
    List.iter
      ~f:(fun b -> Out_channel.fprintf block_file "%s\n" (csv_row block_cols b))
      r.chain ;
    Out_channel.close block_file;
  in
  let queue = ref configs in
  Parany.run n_cores
    ~demux:(fun () ->
        match !queue with
        | [] -> raise Parany.End_of_input
        | hd :: tl -> queue := tl; hd
      )
    ~work:(fun cfg -> cfg, Simulator.simulate io cfg)
    ~mux:(fun (cfg, r) ->
        (* all IO happens in the main process *)
        log_run cfg r;
        write_blocks cfg r;
        progress := !progress + (estimate_time cfg);
        printf "\r%3.0f%%%!" (rational !progress time_estimate *. 100.)
      );
  Out_channel.close runs_file;
  printf "\n"
