open Base

(* Optimization:
   give: orphan rate, quorum sizes
   find: pow scale / block interval *)

let tag = "optim-orphan"
let quorum_size = 32
let target_orphan_rate = 0.01

let min_pow_scale = 0.25
let max_pow_scale = 1. /. min_pow_scale /. target_orphan_rate

let n_nodes = 15
let n_blocks = 500
let n_cores = Cpu.numcores ()

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
    par_sim n_cores n_cores io_params params
  in
  let () =
    Stdio.printf ". yields orphan rate %05.4f.\n%!" orphan_rate
  in
  orphan_rate

let max_pow_scale_point = (max_pow_scale, orphan_rate max_pow_scale)
let min_pow_scale_point = (min_pow_scale, orphan_rate min_pow_scale)

let window ~size ~around d =
  let above =
    Map.to_sequence
      ~order:`Increasing_key
      ~keys_greater_or_equal_to:around
      d
  and below =
    Map.to_sequence
      ~order:`Decreasing_key
      ~keys_less_or_equal_to:around
      d
  in
  let rec collect n above below acc =
    let h (min, max, keys, values) (k, v) = Float.min min k, Float.max max k, k :: keys, v :: values in
    if n > 0 then
      match Sequence.(next above, next below) with
      | Some (a, above), Some (b, below) ->
        collect (n-2) above below (h (h acc b) a)
      | Some (a, above), None ->
        collect (n-2) above below (h (h acc min_pow_scale_point) a)
      | None, Some (b, below) ->
        collect (n-2) above below (h (h acc b) max_pow_scale_point)
      | None, None ->
        collect (n-2) above below (h (h acc min_pow_scale_point) max_pow_scale_point)
    else acc
  in
  let min, max, key, values =
    collect size above below (Float.infinity, Float.neg_infinity, [], [])
  in
  min, max, List.zip_exn key values

type line = {intercept: float; slope:float}

let ols points : line =
  let n, xsum, ysum, xysum, x2sum =
    List.fold_left points
      ~f:(fun (n, xsum, ysum, xysum, x2sum) (x,y) -> n + 1, x +. xsum, y +. ysum, x *. y +. xysum, (x **. 2.) +. x2sum)
      ~init:(0, 0.,0.,0., 0.)
  in
  let n = Float.of_int n in
  let xmean = xsum /. n in
  let slope =
    let a = xysum -. xmean *. ysum
    and b = x2sum -. n *. (xmean **. 2.)
    in a /. b
  in
  let intercept =
    ysum /. n -. slope *. xmean
  in {slope; intercept}

let intersect ~y line = (y -. line.intercept) /. line.slope

let optim () =
  let init = Map.empty (module Float) in
  let rec step n last_estimate d =
    Stdio.printf "estimate: %f\n%!" last_estimate;
    if n > 0 then
      let min, max, points = window ~around:last_estimate ~size:50 d in
      let pow_scale = Random.float_range min max in
      let orphan_rate = orphan_rate pow_scale in
      let d = Map.set ~key:pow_scale ~data:orphan_rate d in
      let line = ols ((pow_scale, orphan_rate) :: points) in
      let next_estimate = intersect ~y:target_orphan_rate line in
      step (n - 1) next_estimate d
  in step 1000 ((min_pow_scale +. max_pow_scale) /. 2.) init

let () = optim ()
