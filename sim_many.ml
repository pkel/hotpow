open Base

(* TODO read from command line *)
(* let n_blocks = 1024 *)
(* let n_nodes = 128 *)
(* let n_iterations = 64 *)
let n_blocks = 512
let n_nodes = 32
let n_iterations = 32
let n_cores = Cpu.numcores ()

let range a b =
  let rec r acc b = if b < a then acc else r (b :: acc) (b - 1) in
  r [] b

let map f l = List.map ~f l
let iter f l = List.iter ~f l
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
  iter
    (fun iteration ->
      let hash = Hashable.hash (id, iteration) in
      match Hashtbl.find tasks hash with
      | None ->
          let data =
            {params; estimate= estimate_time params; tags= [tag]; iteration; id}
          in
          Hashtbl.set tasks ~key:hash ~data
      | Some ct -> ct.tags <- tag :: ct.tags)
    (range 1 n_iterations)

let () =
  (* keep 1 atv per 1 delta fixed, scale quorum size, investigate orphans *)
  let open Simulator in
  range 0 8
  |> map (fun x -> 1 lsl x)
  |> iter (fun quorum_size ->
         let uni =
           { n_nodes
           ; n_blocks
           ; protocol= Parallel
           ; quorum_size
           ; confirmations= 32
           ; pow_scale= 1.
           ; delta_dist= Uniform
           ; delta_vote= 1.
           ; delta_block= 1.
           ; leader_failure_rate= 0.
           ; churn= 0.
           ; eclipse_time= 10.
           ; (* Attacker *)
             alpha= rational 1 n_nodes
           ; strategy= Parallel } in
         schedule ~tag:"fixed-rate" uni ;
         schedule ~tag:"fixed-rate" {uni with delta_dist= Exponential})

let () =
  (* keep quorum size fixed, speed up pow, investigate orphans *)
  let open Simulator in
  range 0 10
  |> map (fun x -> 1 lsl x)
  |> iter (fun speed ->
         let q1_uni =
           { n_nodes
           ; n_blocks
           ; protocol= Parallel
           ; quorum_size= 1
           ; confirmations= 32
           ; pow_scale= rational speed (1 lsl 3)
           ; delta_dist= Uniform
           ; delta_vote= 1.
           ; delta_block= 1.
           ; leader_failure_rate= 0.
           ; churn= 0.
           ; eclipse_time= 10.
           ; (* Attacker *)
             alpha= rational 1 n_nodes
           ; strategy= Parallel } in
         let q1_exp = {q1_uni with delta_dist= Exponential} in
         schedule ~tag:"fixed-quorum" q1_uni ;
         schedule ~tag:"fixed-quorum" q1_exp ;
         schedule ~tag:"fixed-quorum" {q1_uni with quorum_size= 8} ;
         schedule ~tag:"fixed-quorum" {q1_exp with quorum_size= 8} ;
         schedule ~tag:"fixed-quorum" {q1_uni with quorum_size= 64} ;
         schedule ~tag:"fixed-quorum" {q1_exp with quorum_size= 64})

let () =
  (* set orphan rate and quorum size investigate minimum viable block interval *)
  let open Simulator in
  range 0 8
  |> map (fun x -> 1 lsl x)
  |> iter (fun quorum_size ->
         iter
           (fun interval ->
             let cfg =
               { n_nodes
               ; n_blocks
               ; protocol= Parallel
               ; quorum_size
               ; confirmations= 32
               ; pow_scale= rational interval quorum_size
               ; delta_dist= Uniform
               ; delta_vote= 1.
               ; delta_block= 1.
               ; leader_failure_rate= 0.
               ; churn= 0.
               ; eclipse_time= 10.
               ; (* Attacker *)
                 alpha= rational 1 n_nodes
               ; strategy= Parallel } in
             schedule ~tag:"max-orphan-rate" cfg)
           [100; 125; 150; 175; 200; 225; 250; 275; 300])

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
    iter (fun tag -> Out_channel.fprintf runs_file "%s,%s\n%!" (a tag) b) t.tags
  and write_blocks t (r : Simulator.result) =
    let open Simulator in
    let block_file =
      Out_channel.create
        (Printf.sprintf "output/blocks-%08x-%i.csv" t.id t.iteration) in
    Out_channel.fprintf block_file "%s\n" (csv_head block_cols) ;
    List.iter
      ~f:(fun b -> Out_channel.fprintf block_file "%s\n" (csv_row block_cols b))
      r.blocks ;
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
