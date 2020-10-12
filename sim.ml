let () = Random.self_init ()

open Primitives

type distribution = Exponential | Uniform

let string_of_distribution = function
  | Exponential -> "exponential"
  | Uniform -> "uniform"

let distribution_enum =
  List.map (fun d -> (string_of_distribution d, d)) [Exponential; Uniform]

let draw d p =
  match d with
  | Uniform -> p *. Random.float 2.
  | Exponential -> -1. *. p *. log (Random.float 1.)

type strategy = Linear | Parallel | Hotpow | Hotpow_censor

let strategy_enum = [Linear; Parallel; Hotpow; Hotpow_censor]

let string_of_strategy = function
  | Linear -> "linear"
  | Parallel -> "parallel"
  | Hotpow -> "hotpow"
  | Hotpow_censor -> "hotpow-censor"

let implementation_of_strategy : strategy -> (module Implementation) = function
  | Linear -> failwith "linear strategy not implemented"
  | Parallel -> (module Prot_parallel)
  | Hotpow -> (module Hotpow)
  | Hotpow_censor -> (module Hotpow_censor)

let strategy_enum = List.map (fun s -> (string_of_strategy s, s)) strategy_enum

include struct
  [@@@ocaml.warning "-39"]

  type params =
    { n_blocks: int [@default 1024] [@aka ["b"]]
          (** Set amount of blocks to simulate. *)
    ; n_nodes: int [@default 16] [@aka ["n"]]
          (** Set number of nodes in the network. *)
    ; protocol: strategy [@default Parallel] [@aka ["p"]] [@enum strategy_enum]
          (** Set the protocol, i.e. the strategy used by honest nodes. *)
    ; confirmations: int [@default 3] [@aka ["k"]]
          (** Set the number of confirmations needed for accepting the payload of a block. Updates deeper than k are considered inconsistencies. *)
    ; quorum_size: int [@default 8] [@aka ["q"]]
          (** Set the quorum size. Does not apply to all protocols. *)
    ; strategy: strategy [@default Parallel] [@aka ["s"]] [@enum strategy_enum]
          (** Set the attacker's strategy. *)
    ; alpha: float [@default 1. /. 16.] [@aka ["a"]]
          (** Set adversaries relative compute power. *)
    ; delta_dist: distribution
          [@default Exponential] [@aka ["d"]] [@enum distribution_enum]
          (** Set the distribution of propagation delays. *)
    ; delta_vote: float [@default 0.]
          (** Set the expected vote propagation delay. *)
    ; delta_block: float [@default 0.]
          (** Set the expected block propagation delay. *)
    ; eclipse_time: float [@default 10.]
          (** Set how long (multiple of expected block time) nodes are eclipsed
              from the network. Messages sent by or delivered to eclipsed nodes
              are delayed until the end of the eclipse. *)
    ; churn: float [@default 0.] [@aka ["c"]]
          (** Set how many (relative) nodes are eclipsed from the network. As
              soon as one eclipse ends (after eclipse-time), another random
              (non-attacker) node is eclipsed.
              *)
    ; leader_failure_rate: float [@default 0.] [@aka ["f"]]
          (** Set the probability of a truthful leader failing to propose a
              block. We model leader failure by suppressing block proposals. *)
    ; header: bool  (** Print csv headers and exit. *)
    ; progress: bool
          (** Print intermediate results to STDERR. Constructing the
              intermediate results creates a significant overhead. *)
    ; verbosity: int [@aka ["v"]] [@default 0]
          (** Print events. > 0 : Message send; > 1 : ATV assignments;
              > 2 : Message delivery *)
    }
  [@@deriving cmdliner]
end

let check_params p =
  let fail p msg =
    Printf.eprintf "Invalid parameter --%s: %s\n%!" p msg ;
    exit 1 in
  if p.n_nodes < 2 then fail "n-blocks" "must be >= 2" ;
  if p.n_blocks < 1 then fail "n-blocks" "must be >= 1" ;
  if p.quorum_size < 1 then fail "quorum-size" "must be >= 1" ;
  if p.alpha < 0. || p.alpha > 1. then fail "alpha" "must be in [0,1]" ;
  if p.delta_vote < 0. then fail "delta-vote" "must be >= 0" ;
  if p.delta_block < 0. then fail "delta-block" "must be >= 0" ;
  if p.eclipse_time <= 0. then fail "eclipse-time" "must be > 0" ;
  if p.churn < 0. || p.churn > 1. then fail "churn" "must be in [0,1]" ;
  if p.leader_failure_rate < 0. || p.leader_failure_rate > 1. then
    fail "leader-failure-rate" "must be in [0,1]"

type result =
  { block_cnt: int
  ; attacker_block_cnt: int
  ; attacker_vote_cnt: int
  ; attacker_block_share: float
  ; attacker_vote_share: float
  ; branches: int
  ; branch_depth: int
  ; atv_cnt: int
  ; max_vote: float
  ; max_vote_mean: float
  ; max_vote_sd: float
  ; efficiency: float
  ; block_time: float }

type 'a column = {title: string; f: 'a -> string}
type row = {p: params; r: result}

let cols : row column list =
  let i = string_of_int
  and f = string_of_float
  and s = string_of_strategy
  and d = string_of_distribution in
  [ {title= "p.protocol"; f= (fun x -> s x.p.protocol)}
  ; {title= "p.confirmations"; f= (fun x -> i x.p.confirmations)}
  ; {title= "p.quorum_size"; f= (fun x -> i x.p.quorum_size)}
  ; {title= "p.n_blocks"; f= (fun x -> i x.p.n_blocks)}
  ; {title= "p.n_nodes"; f= (fun x -> i x.p.n_nodes)}
  ; {title= "p.alpha"; f= (fun x -> f x.p.alpha)}
  ; {title= "p.strategy"; f= (fun x -> s x.p.strategy)}
  ; {title= "p.delta_dist"; f= (fun x -> d x.p.delta_dist)}
  ; {title= "p.delta_vote"; f= (fun x -> f x.p.delta_vote)}
  ; {title= "p.delta_block"; f= (fun x -> f x.p.delta_block)}
  ; {title= "p.churn"; f= (fun x -> f x.p.churn)}
  ; {title= "p.eclipse_time"; f= (fun x -> f x.p.eclipse_time)}
  ; {title= "p.leader_failure_rate"; f= (fun x -> f x.p.leader_failure_rate)}
  ; {title= "r.branches"; f= (fun x -> i x.r.branches)}
  ; {title= "r.branch_depth"; f= (fun x -> i x.r.branch_depth)}
  ; {title= "r.block_cnt"; f= (fun x -> i x.r.block_cnt)}
  ; {title= "r.attacker_block_cnt"; f= (fun x -> i x.r.attacker_block_cnt)}
  ; {title= "r.attacker_vote_cnt"; f= (fun x -> i x.r.attacker_vote_cnt)}
  ; {title= "r.attacker_block_share"; f= (fun x -> f x.r.attacker_block_share)}
  ; {title= "r.attacker_vote_share"; f= (fun x -> f x.r.attacker_vote_share)}
  ; {title= "r.atv_cnt"; f= (fun x -> i x.r.atv_cnt)}
  ; {title= "r.max_vote"; f= (fun x -> f x.r.max_vote)}
  ; {title= "r.max_vote_mean"; f= (fun x -> f x.r.max_vote_mean)}
  ; {title= "r.max_vote_sd"; f= (fun x -> f x.r.max_vote_sd)}
  ; {title= "r.efficiency"; f= (fun x -> f x.r.efficiency)}
  ; {title= "r.block_time"; f= (fun x -> f x.r.block_time)} ]

let cols_progress : row column list =
  let i = string_of_int and f = string_of_float in
  [ {title= "r.block_cnt"; f= (fun x -> i x.r.block_cnt)}
  ; {title= "r.branches"; f= (fun x -> i x.r.branches)}
  ; {title= "r.branch_depth"; f= (fun x -> i x.r.branch_depth)}
  ; {title= "r.attacker_block_share"; f= (fun x -> f x.r.attacker_block_share)}
  ; {title= "r.attacker_vote_share"; f= (fun x -> f x.r.attacker_vote_share)}
  ; {title= "r.block_time"; f= (fun x -> f x.r.block_time)} ]

let csv_head cols = String.concat "," (List.map (fun x -> x.title) cols)
let csv_row cols row = String.concat "," (List.map (fun x -> x.f row) cols)

type net_event =
  | Broadcast of {src: int; cnt: int; m: message}
  | Deliver of {src: int; rcv: int; cnt: int; m: message}

type event = ATV of {nth: int; node: int} | Net of net_event | Shutdown
type eclipse = {till: float; queue: net_event Queue.t}
type node = {m: (module Node); mutable eclipse: eclipse option}

type state =
  { mutable height: int
  ; mutable atv_cnt: int
  ; mutable shutdown: bool
  ; attacker_id: DSA.public_key
  ; attacker_secret: DSA.private_key
  ; atv_rate: float
  ; nodes: node array }

let string_of_block b =
  let open Link in
  Printf.sprintf "Block %s->%s" (hash b |> to_string) (to_string b.parent)

let string_of_vote (lnk, _, _) =
  Printf.sprintf "Vote for %s" (Link.to_string lnk)

let string_of_message = function
  | Block b -> string_of_block b
  | Vote v -> string_of_vote v

let string_of_event =
  let open Printf in
  function
  | Net (Broadcast {src; cnt; m}) ->
      sprintf "n%i sends m%i: %s" src cnt (string_of_message m)
  | Net (Deliver {rcv; cnt; m; _}) ->
      sprintf "deliver m%i to n%i: %s" cnt rcv (string_of_message m)
  | ATV {nth; node} -> sprintf "assign %ith ATV to n%i" nth node
  | Shutdown -> sprintf "stop simulation"

module Event = struct
  (* Imperative interface for functional event queue *)
  open Event_queue

  let eq : event t ref = ref empty
  let time = ref 0.
  let now () = !time
  let schedule ?(delay = 0.) event = eq := schedule !eq (!time +. delay) event

  let next () =
    let time', event, eq' = next !eq in
    time := time' ;
    eq := eq' ;
    (time', event)

  let empty () = !eq = empty
  let time = ()
  let eq = ()
end

let rec eclipse_random_node till nodes =
  (* Pick a node but not the attacker *)
  let i = Random.int (Array.length nodes - 1) + 1 in
  match nodes.(i).eclipse with
  | Some _ -> eclipse_random_node till nodes
  | None -> nodes.(i).eclipse <- Some {queue= Queue.create (); till}

(* Get time of next ATV and assign random ATV recipient. *)
let schedule_atv ~p ~s =
  let delay = draw Exponential (1. /. s.atv_rate)
  and node =
    if Random.float 1. <= p.alpha then 0
    else Random.int (Array.length s.nodes - 1) + 1 in
  Event.schedule ~delay (ATV {nth= s.atv_cnt; node})

let handle_event ~p ~s =
  let handle_net = function
    | Broadcast {m= Block _; src; _}
      when src > 0 && Random.float 1. <= p.leader_failure_rate ->
        (* leader fails. *) ()
    | Broadcast {src; cnt; m} ->
        let lat =
          match m with Vote _ -> p.delta_vote | Block _ -> p.delta_block in
        Array.iteri
          (fun rcv _ ->
            if rcv <> src then
              let delay = if lat > 0. then draw p.delta_dist lat else 0. in
              Event.schedule ~delay (Net (Deliver {src; rcv; cnt; m})))
          s.nodes
    | Deliver x ->
        let n = s.nodes.(x.rcv) in
        let (module N) = n.m in
        let _fresh = N.on_receive x.m in
        () in
  let uneclipse n =
    match n.eclipse with
    | None -> ()
    | Some eclipse ->
        Queue.iter handle_net eclipse.queue ;
        n.eclipse <- None in
  function
  | ATV {node; nth} ->
      if not s.shutdown then (
        let (module N : Node) = s.nodes.(node).m in
        s.atv_cnt <- s.atv_cnt + 1 ;
        N.on_atv nth ;
        schedule_atv ~p ~s )
  | Shutdown -> Array.iter uneclipse s.nodes
  | Net ev -> (
      let actor =
        match ev with Broadcast {src; _} -> src | Deliver {rcv; _} -> rcv in
      let n = s.nodes.(actor) in
      match n.eclipse with
      | None -> handle_net ev
      | Some eclipse ->
          let now = Event.now () in
          if now < eclipse.till then Queue.push ev eclipse.queue
          else (
            uneclipse n ;
            handle_net ev ;
            eclipse_random_node (now +. p.eclipse_time) s.nodes ) )

let quorum_threshold = Weight.max_weight

let broadcast =
  let message_cnt = ref 0 in
  fun nr ->
    let module B = struct
      let send m =
        incr message_cnt ;
        Event.schedule (Net (Broadcast {cnt= !message_cnt; m; src= nr}))
    end in
    (module B : Broadcast)

let spawn ~p id secret strategy =
  let module Config = struct
    let quorum_size = p.quorum_size
    let quorum_threshold = quorum_threshold
    let confirmations = p.confirmations
    let my_id = id
    let my_secret = secret
  end in
  let (module Broadcast) = broadcast DSA.(int_of_id id)
  and (module Implementation) = implementation_of_strategy strategy in
  (module Implementation.Spawn (Broadcast) (Config) : Node)

module StateTree : sig
  type t
  type state = int
  type history = state list

  val add : history -> t -> t
  val of_seq : history Seq.t -> t
  val empty : t
  val branches : t -> int
  val depth : t -> int
  val print : t -> unit
end = struct
  type state = int
  type history = state list
  type node = {state: state; children: node list}
  type t = node list

  let empty = []

  let rec print lvl nodes = List.iter (print' lvl) nodes

  and print' lvl node =
    Printf.printf "%i: %i\n%!" lvl node.state ;
    print (lvl + 1) node.children

  let print = print 0

  let rec branches = function
    | [] -> 1
    | l -> List.fold_left (fun acc el -> branches el.children + acc) 0 l

  (* drop consistent history *)
  let rec compress = function
    | [] -> []
    | [node] -> compress node.children
    | x -> x

  let rec height acc nodes =
    List.fold_left (fun m n -> max m (height (acc + 1) n.children)) acc nodes

  let height = height 0
  let depth t = compress t |> height

  let rec merge neq history remaining =
    match (history, remaining) with
    | [], _ -> List.rev_append neq remaining
    | state :: history, [] -> {state; children= merge [] history []} :: neq
    | state :: history, node :: remaining when node.state = state ->
        ({node with children= merge [] history node.children} :: neq)
        @ remaining
    | state :: history, node :: remaining ->
        merge (node :: neq) (state :: history) remaining

  let add = merge []
  let of_seq = Seq.fold_left (fun acc el -> add el acc) empty
end

(* let () = *)
(* let gen l = List.to_seq l |> StateTree.of_seq in *)
(* let a = gen [[1; 2; 3; 4]; [1; 2; 3; 4]; [1; 2; 3]; [1; 2; 4]] *)
(* and b = gen [[1; 2; 3; 4]; [1; 2; 3; 4; 5]; [1]; []] *)
(* and c = gen [[1; 2; 3; 4]; [1; 3; 2; 4]; [1; 3; 2]; [1; 4; 3; 2]] *)
(* and d = gen [[1; 2; 3; 4]; [1; 2; 3; 3]] *)
(* in *)
(* print_endline "a" ; *)
(* StateTree.print a ; *)
(* print_endline "b" ; *)
(* StateTree.print b ; *)
(* print_endline "c" ; *)
(* StateTree.print c ; *)
(* assert (StateTree.branches a = 2) ; *)
(* assert (StateTree.branches b = 1) ; *)
(* assert (StateTree.branches c = 3) ; *)
(* assert (StateTree.branches d = 2) ; *)
(* Printf.printf "depth: %i\n%!" (StateTree.depth a); *)
(* Printf.printf "depth: %i\n%!" (StateTree.depth b); *)
(* Printf.printf "depth: %i\n%!" (StateTree.depth c); *)
(* Printf.printf "depth: %i\n%!" (StateTree.depth d); *)
(* assert (StateTree.depth a = 2) ; *)
(* assert (StateTree.depth b = 0) ; *)
(* assert (StateTree.depth c = 3) ; *)
(* assert (StateTree.depth d = 1) *)

(** compare states, count versions, ignore the latest addition which might be
    out of sync *)
let branch_analysis nodes =
  let open App in
  let f n =
    let (module N : Node) = n.m in
    N.get_state () in
  let states = Array.map f nodes in
  let history state =
    List.rev_map (fun c -> Hashtbl.hash c.hash) state.entries in
  Array.map history states |> Array.to_seq |> StateTree.of_seq
  |> fun st -> StateTree.(branches st, depth st)

let count_leadership id (module N : Node) =
  let open App in
  let cnt = ref 0 in
  let check_lead t = fst (List.nth t.quorum 0) = id in
  List.iter (fun q -> if check_lead q then incr cnt) (N.get_state ()).entries ;
  !cnt

let count_votes id (module N : Node) =
  let open App in
  let cnt = ref 0 in
  List.iter
    (fun t -> List.iter (fun v -> if fst v = id then incr cnt) t.quorum)
    (N.get_state ()).entries ;
  !cnt

let max_vote_weight_stats (module N : Node) =
  let open App in
  let weights =
    (N.get_state ()).entries
    |> List.map (fun t ->
           List.(nth (rev t.quorum) 0)
           |> fun (id, s) -> Weight.weigh (t.parent, id, s) |> float_of_int)
  in
  let n, mean, max =
    let n, sum, max =
      List.fold_left
        (fun (n, sum, m) w -> (n + 1, w +. sum, max w m))
        (0, 0., 0.) weights in
    (n, sum /. float_of_int n, max) in
  let sd =
    let esum =
      List.fold_left (fun esum w -> ((w -. mean) ** 2.) +. esum) 0. weights
    in
    sqrt (esum /. float_of_int n) in
  (max, mean, sd)

let get_height (module N : Node) = (N.get_state ()).height

let from_uneclipsed get nodes =
  let rec f seq =
    match seq () with
    | Seq.Nil -> get nodes.(1).m
    | Cons (n, seq) -> (
      match n.eclipse with None -> get n.m | Some _ -> f seq ) in
  f Array.(sub nodes 1 (length nodes - 1) |> to_seq)

let ratio a b = float_of_int a /. float_of_int b

let result ~p ~s =
  let max_vote, max_vote_mean, max_vote_sd =
    from_uneclipsed max_vote_weight_stats s.nodes
  and block_time = Event.now () /. float_of_int (s.height + 3)
  and efficiency =
    ratio ((s.height + 3) * p.quorum_size) s.atv_cnt
    (* plus three because 3 blocks are not yet committed *) in
  let attacker_block_cnt =
    from_uneclipsed (count_leadership s.attacker_id) s.nodes
  and attacker_vote_cnt = from_uneclipsed (count_votes s.attacker_id) s.nodes
  and block_cnt = s.height
  and branches, branch_depth = branch_analysis s.nodes in
  { block_cnt
  ; attacker_block_cnt
  ; attacker_block_share= ratio attacker_block_cnt block_cnt
  ; attacker_vote_cnt
  ; attacker_vote_share= ratio attacker_vote_cnt (block_cnt * p.quorum_size)
  ; branches
  ; branch_depth
  ; max_vote_mean
  ; max_vote_sd
  ; max_vote
  ; atv_cnt= s.atv_cnt
  ; efficiency
  ; block_time }

let init ~p =
  let attacker_id, attacker_secret = DSA.generate_id () in
  let atv_rate = float_of_int p.quorum_size
  and nodes : node array =
    Array.init p.n_nodes (fun i ->
        let m =
          if i = 0 then spawn ~p attacker_id attacker_secret p.strategy
          else
            let id, secret = DSA.generate_id () in
            spawn ~p id secret p.protocol in
        {m; eclipse= None}) in
  let () =
    (* eclipse first set of nodes *)
    let n = int_of_float (floor (float_of_int p.n_nodes *. p.churn)) in
    let d = p.eclipse_time /. float_of_int n in
    let rec eclipse n =
      if n = 0 then ()
      else (
        eclipse_random_node (float_of_int n *. d) nodes ;
        eclipse (n - 1) ) in
    eclipse n in
  { height= 0
  ; atv_cnt= 0
  ; shutdown= false
  ; attacker_id
  ; attacker_secret
  ; atv_rate
  ; nodes }

let event_filter verbosity = function
  | Net (Deliver _) when verbosity >= 3 -> true
  | ATV _ when verbosity >= 2 -> true
  | Net (Broadcast _) when verbosity >= 1 -> true
  | _ -> false

let simulate p =
  let s = init ~p in
  let log t e =
    if event_filter p.verbosity e then
      Printf.printf "%14.5f    %s\n%!" t (string_of_event e) in
  if p.progress then Printf.eprintf "%s\n%!" (csv_head cols_progress) ;
  schedule_atv ~p ~s ;
  while not (Event.empty ()) do
    let t, e = Event.next () in
    let () =
      match e with
      | Net (Broadcast {m= Block _; src; _}) ->
          s.height <- max (get_height s.nodes.(src).m) s.height ;
          if s.height >= p.n_blocks && not s.shutdown then (
            s.shutdown <- true ;
            Event.schedule Shutdown )
      | _ -> () in
    handle_event ~p ~s e ; log t e
  done ;
  if p.progress then Printf.eprintf "\n%!" ;
  result ~p ~s

let term =
  let f p =
    check_params p ;
    if p.header then (
      print_endline (csv_head cols) ;
      exit 0 ) ;
    let row = {p; r= simulate p} in
    print_endline (csv_row cols row) in
  Cmdliner.Term.(const f $ params_cmdliner_term ())

let info = Cmdliner.(Term.info "sim" ~doc:"HotPow Simulator")
let () = Cmdliner.(Term.exit @@ Term.eval (term, info))
