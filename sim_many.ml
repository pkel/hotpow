open Base

(* TODO read from command line *)
let n_blocks = 1024
let n_nodes = 32 (* 1024 *)
let n_iterations = 16 (* 64 *)
let n_confirmations = 32
let n_cores = Cpu.numcores ()

let range a b =
  let rec r acc b = if b < a then acc else r (b :: acc) (b - 1) in
  r [] b

let map f l = List.map ~f l
let iter l f = List.iter ~f l
let foldl f init l = List.fold_left ~f ~init l
let rational a b = Float.of_int a /. Float.of_int b

let estimate_time cfg =
  let open Simulator in
  cfg.quorum_size * cfg.n_blocks * cfg.n_nodes

type task =
  { params: Simulator.params
  ; id: int
  ; (* params hash *)
    estimate: int
  ; mutable tags: string list
  ; iteration: int }

let tasks = Hashtbl.create (module Int)

let check_params ~tag p =
  let open Stdio in
  match Simulator.check_params p with
  | Ok _ -> ()
  | Error s ->
      eprintf "Error in configuration tagged %s: %s\n%s\n%!" tag s
        (Simulator.show_params p) ;
      Caml.exit 1

let schedule ~tag params =
  let () = check_params ~tag params in
  let id = Simulator.hash_params params in
  iter (range 1 n_iterations)
    (fun iteration ->
      let hash = Hashable.hash (id, iteration) in
      match Hashtbl.find tasks hash with
      | None ->
          let data =
            {params; estimate= estimate_time params; tags= [tag]; iteration; id}
          in
          Hashtbl.set tasks ~key:hash ~data
      | Some ct -> ct.tags <- tag :: ct.tags)

(* synchrony assumption: Δ = 2s *)
let synchrony = 2.

(* Three configurations:
 * - k = 51, lambda = k/600 , votes 250ms, blocks 2s
 * - k = 1 , lambda = k/600 , votes n/a  , blocks 2s
 * - k = 1 , lambda = 51/600, votes n/a  , blocks 500ms
 *)

let scenarios =
  (* protocol, confirmations, k, lambda, delta_vote, delta_block *)
  [ "proposed",  1, 51, rational 600 51, synchrony /. 8., synchrony
  ; "nc-slow",  10,  1, rational 600  1, synchrony      , synchrony
  ; "nc-fast",  30,  1, rational 600 51, synchrony      , synchrony /. 4.
  ]

(* base config *)
let base =
  let open Simulator in
  { n_nodes
  ; n_blocks
  ; protocol= Parallel
  ; quorum_size= 0
  ; confirmations= 0
  ; pow_scale = synchrony
  ; delta_dist = Uniform
  ; delta_vote= synchrony
  ; delta_block= synchrony
  ; leader_failure_rate= 0.
  ; churn= 0.
  ; eclipse_time= 3600. (* 1h *)
  ; (* Attacker *)
    alpha= rational 1 n_nodes
  ; strategy= Parallel
  }

let simplified (s, confirmations, quorum_size, pow_scale, _, _) =
  s, { base with quorum_size
               ; confirmations
               ; pow_scale
               ; delta_vote= synchrony
               ; delta_block= synchrony
     }

let realistic (s, confirmations, quorum_size, pow_scale, delta_vote, delta_block) =
  s, { base with quorum_size
               ; confirmations
               ; pow_scale
               ; delta_vote
               ; delta_block
     }

(* All scenarios, simplified networks *)
let () =
  iter scenarios (fun s ->
      let s, cfg = simplified s in
      schedule ~tag:("simplified-exponential-" ^ s)
        {cfg with delta_dist=Exponential};
      schedule ~tag:("simplified-uniform-" ^ s)
        {cfg with delta_dist=Uniform}
    )

(* All scenarios, realistic networks *)
let () =
  iter scenarios (fun s ->
      let s, cfg = realistic s in
      schedule ~tag:("realistic-exponential-" ^ s)
        { cfg with delta_dist=Exponential};
      schedule ~tag:("realistic-uniform-" ^ s)
        { cfg with delta_dist=Uniform}
    )

(* All scenarios, simplified networks, varying expected latency. *)
let () =
  let deltas =
    range 0 6 |> map (fun x -> rational (1 lsl x) 4) (* 1/4 ... 16 *)
  in
  iter deltas (fun d ->
      iter scenarios (fun s ->
          let s, cfg = simplified s in
          let cfg =
            { cfg with delta_vote= d
                     ; delta_block= d
            }
          in
          schedule ~tag:("latency-simplified-exponential-" ^ s)
            { cfg with delta_dist=Exponential};
          schedule ~tag:("latency-simplified-uniform-" ^ s)
            { cfg with delta_dist=Uniform}
        ))

(* All scenarios, realistic networks, varying churn ratios. *)
let () =
  let churns =
    range 0 5 |> map (fun x -> rational x 10)
  in
  iter churns (fun churn ->
      iter scenarios (fun s ->
          let s, cfg = realistic s in
          let cfg = { cfg with churn } in
          schedule ~tag:("churn-realistic-exponential-" ^ s)
            { cfg with delta_dist=Exponential};
          schedule ~tag:("churn-realistic-uniform-" ^ s)
            { cfg with delta_dist=Uniform}
        ))

(* All scenarios, realistic networks, varying leader failure rates. *)
let () =
  let failures =
    range 0 5 |> map (fun x -> rational x 10)
  in
  iter failures (fun leader_failure_rate ->
      iter scenarios (fun s ->
          let s, cfg = realistic s in
          let cfg = { cfg with leader_failure_rate } in
          schedule ~tag:("failure-realistic-exponential-" ^ s)
            { cfg with delta_dist=Exponential};
          schedule ~tag:("failure-realistic-uniform-" ^ s)
            { cfg with delta_dist=Uniform}
        ))

let run_cols : (string * task) Simulator.column list =
  let open Simulator.ToString in
  [ {title= "tag"; f= fst}
  ; {title= "id"; f= (fun (_, t) -> Printf.sprintf "%08x" t.id)}
  ; {title= "iteration"; f= (fun (_, t) -> i t.iteration)} ]

let () =
  let open Stdio in
  let tasks = Hashtbl.data tasks in
  printf "Simulating %i configurations...\n%!" (List.length tasks) ;
  let time_estimate =
    List.fold_left ~init:0 ~f:( + ) (map (fun c -> c.estimate) tasks) in
  let progress = ref 0 in
  let io = Simulator.{verbosity= 0} in
  let runs_file = Out_channel.create "output/runs.csv" in
  Out_channel.fprintf runs_file "%s,%s\n%!"
    (Simulator.csv_head run_cols)
    Simulator.(csv_head cols) ;
  let log_run t r =
    let a tag = Simulator.csv_row run_cols (tag, t) in
    let b = Simulator.(csv_row cols {p= t.params; r}) in
    iter t.tags (fun tag -> Out_channel.fprintf runs_file "%s,%s\n%!" (a tag) b)
  and write_blocks t (r : Simulator.result) =
    let open Simulator in
    let block_file =
      Out_channel.create
        (Printf.sprintf "output/blocks-%08x-%i.csv" t.id t.iteration) in
    Out_channel.fprintf block_file "%s\n" (csv_head block_cols) ;
    iter r.blocks
      (fun b -> Out_channel.fprintf block_file "%s\n" (csv_row block_cols b));
    Out_channel.close block_file in
  let queue = ref tasks in
  printf "%3.0f%%%!" 0. ;
  Parany.run n_cores
    ~demux:(fun () ->
      match !queue with
      | [] -> raise Parany.End_of_input
      | hd :: tl ->
          queue := tl ;
          hd)
    ~work:(fun task -> (task, Simulator.simulate io task.params))
    ~mux:(fun (task, r) ->
      (* all IO happens in the main process *)
      log_run task r ;
      write_blocks task r ;
      progress := !progress + task.estimate ;
      printf "\r%3.0f%%%!" (rational !progress time_estimate *. 100.)) ;
  Out_channel.close runs_file ;
  printf "\n"
