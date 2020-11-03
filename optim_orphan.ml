open Base

(* Optimization:
   give: orphan rate, quorum sizes
   find: pow scale / block interval *)

let tag = "optim-orphan"
let quorum_size = 32
let target_orphan_rate = 0.01
let n_nodes = 100
let n_blocks = 2000
let n_cores = Cpu.numcores ()

let population_size = (4, 4)
let min_pow_scale = 0.25
let max_pow_scale = 1. /. min_pow_scale /. target_orphan_rate

type chromosome = { pow_scale: float; fitness: float; female: bool }

let fitness orphan_rate =
  let open Float in
  1. /. ((orphan_rate -. target_orphan_rate) ** 2.)

let par_sim n_cores n_times io params =
  let n = ref n_times in
  let observed = ref 0 in
  let confirmed = ref 0 in
  Parany.run n_cores
    ~demux:(fun () ->
        if !n <= 0 then raise Parany.End_of_input
        else Int.decr n)
    ~work:(fun () -> Simulator.simulate io params)
    ~mux:(fun r ->
        observed := !observed + r.votes_observed;
        confirmed := !confirmed + r.votes_confirmed;
      );
  (!observed - !confirmed) // !observed

let orphan_rate pow_scale =
  let () =
    Stdio.printf "simulate pow scale %08.4f%!" pow_scale
  in
  let orphan_rate =
    let open Simulator in
    let io_params = {verbosity =0}
    and params =
      { n_nodes
      ; n_blocks = (n_blocks / n_cores)
      ; protocol= Parallel
      ; quorum_size
      ; confirmations= 16
      ; pow_scale
      ; delta_dist= Uniform
      ; delta_vote= 1.
      ; delta_block= 1.
      ; leader_failure_rate= 0.
      ; churn= 0.
      ; eclipse_time= 10.
      ; (* Attacker *)
        alpha= 1 // n_nodes
      ; strategy= Parallel
      } in
    par_sim n_cores n_cores  io_params params
  in
  let () =
    Stdio.printf ". yields orphan rate %05.4f and fitness %.0f.\n%!" orphan_rate (fitness orphan_rate)
  in
  orphan_rate

let chromosome pow_scale =
  let orphan_rate = orphan_rate pow_scale in
  let female =
    if Float.is_negative (orphan_rate -. target_orphan_rate) then true else false
  in
  { pow_scale; fitness = fitness orphan_rate; female}

let random () =
  Random.float_range min_pow_scale max_pow_scale

type population = {female: chromosome array; male: chromosome array}

let population =
  let female =
    let c = chromosome max_pow_scale in
    let () =
      if not c.female then failwith "max.pow.scale did not produce female"
    in
    Array.create ~len:(fst population_size) c
  and male =
    let c = chromosome min_pow_scale in
    let () =
      if c.female then failwith "min.pow.scale did not produce male"
    in
    Array.create ~len:(snd population_size) c
  in
  { male; female}

let random_index arr =
  Random.int (Array.length arr)

let random_element arr =
  arr.(random_index arr)

let fit_element arr =
  let overall_fitness = Array.fold ~init:0. ~f:(fun acc c -> c.fitness +. acc) arr in
  let i = ref (random_index arr) in
  let pick = Random.float overall_fitness in
  let passed = ref 0. in
  let m = Array.length arr in
  while Float.is_negative (!passed -. pick) do
    passed := !passed +. arr.(!i).fitness;
    i := (!i + 1) % m
  done;
  arr.(!i)

let crossover () =
  let f = fit_element population.female
  and m = fit_element population.male
  in
  let a = Random.float 1. in
  let pow_scale = (a *. f.pow_scale +. (1. -. a) *. m.pow_scale) in
  pow_scale

let replace (c: chromosome) =
  let arr = if c.female then population.female else population.male in
  arr.(random_index arr) <- c

let mutate _chromosome = random ()

let () =
  for _i = 1 to 100 do
    crossover ()
    |> chromosome
    |> replace
  done
