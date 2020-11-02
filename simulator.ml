let () = Random.self_init ()

open Primitives
module BlockStore = Prot_parallel.BlockStore

type distribution = Exponential | Uniform
[@@deriving hash, show {with_path= false}]

let string_of_distribution = function
  | Exponential -> "exponential"
  | Uniform -> "uniform"

let distribution_enum =
  List.map (fun d -> (string_of_distribution d, d)) [Exponential; Uniform]

let draw d p =
  match d with
  | Uniform -> p *. Random.float 2.
  | Exponential -> -1. *. p *. log (Random.float 1.)

type strategy = Parallel | Hotpow | Hotpow_censor
[@@deriving hash, show {with_path= false}]

let strategy_enum = [Parallel; Hotpow; Hotpow_censor]

let string_of_strategy = function
  | Parallel -> "parallel"
  | Hotpow -> "hotpow"
  | Hotpow_censor -> "hotpow-censor"

let implementation_of_strategy : strategy -> (module Implementation) = function
  | Parallel -> (module Prot_parallel)
  | Hotpow -> (module Prot_commit)
  | Hotpow_censor -> (module Attack_commit_censor)

let strategy_enum = List.map (fun s -> (string_of_strategy s, s)) strategy_enum

include struct
  [@@@ocaml.warning "-39"]

  open Base

  type params =
    { n_blocks: int [@default 1024] [@aka ["b"]]
          (** Set amount of blocks to simulate. *)
    ; n_nodes: int [@default 16] [@aka ["n"]]
          (** Set number of nodes in the network. *)
    ; protocol: strategy [@default Parallel] [@aka ["p"]] [@enum strategy_enum]
          (** Set the protocol, i.e. the strategy used by honest nodes. *)
    ; confirmations: int [@default 3] [@aka ["k"]]
          (** Set the number of confirmations needed for accepting the payload
              of a block. Updates deeper than k are considered inconsistencies. *)
    ; quorum_size: int [@default 8] [@aka ["q"]]  (** Set the quorum size. *)
    ; pow_scale: float [@default 1.]
          (** Set the expected puzzle solving time, i.e. the scale of the
              exponential distribution driving proof-of-work. In case of
              difficulty adjustment, this is used as start value. *)
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
    ; orphan_rate: float option
          (** Enable difficulty adjustment (moving average) and target the given
              vote orphan rate. *)
    ; da_window: int [@default 32]
          (** Difficulty adjustment is based on the intervals of the last
              [da-window] confirmed blocks. *)
    ; eclipse_time: float [@default 10.]
          (** Set how long (multiple of expected block time) nodes are eclipsed
              from the network. Messages sent by or delivered to eclipsed nodes
              are delayed until the end of the eclipse. *)
    ; churn: float [@default 0.] [@aka ["c"]]
          (** Set how many (relative) nodes are eclipsed from the network. As
              soon as one eclipse ends (after eclipse-time), another random
              (non-attacker) node is eclipsed. *)
    ; leader_failure_rate: float [@default 0.] [@aka ["f"]]
          (** Set the probability of a truthful leader failing to propose a
              block. We model leader failure by suppressing block proposals. *)
    }
  [@@deriving cmdliner, hash, show {with_path= false}]

  type io_params =
    { verbosity: int [@aka ["v"]] [@default 0]
          (** Print events. > 0 : Message send; > 1 : ATV assignments; > 2 :
              Message delivery *) }
  [@@deriving cmdliner]
end

let check_params p =
  let fail p msg =
    let m = Printf.sprintf "%s %s" p msg in
    failwith m in
  try
    if p.n_nodes < 2 then fail "n-nodes" "must be >= 2" ;
    if p.n_blocks < 1 then fail "n-blocks" "must be >= 1" ;
    if p.confirmations < 1 then fail "confirmations" "must be >= 1" ;
    if p.quorum_size < 1 then fail "quorum-size" "must be >= 1" ;
    if p.alpha < 0. || p.alpha > 1. then fail "alpha" "must be in [0,1]" ;
    if p.delta_vote < 0. then fail "delta-vote" "must be >= 0" ;
    if p.delta_block < 0. then fail "delta-block" "must be >= 0" ;
    if p.eclipse_time <= 0. then fail "eclipse-time" "must be > 0" ;
    if p.churn < 0. || p.churn > 1. then fail "churn" "must be in [0,1]" ;
    if p.leader_failure_rate < 0. || p.leader_failure_rate > 1. then
      fail "leader-failure-rate" "must be in [0,1]" ;
    ( match p.orphan_rate with
    | Some r when r < 0. || r >= 1. -> fail "orphan-rate" "must be in [0, 1)"
    | _ -> () ) ;
    if p.da_window < 1 then fail "da-window" "must be > 0" ;
    Ok p
  with Failure m -> Error m

type sim_vote =
  { data: vote
  ; target_height: int (* data.ref.height + 1 *)
  ; published_at: float
  ; mutable confirmed: bool (* modified after simulation *) }

type sim_block =
  { hash: block Link.t
  ; data: block
  ; height: int
  ; published_at: float
  ; mutable confirmed: bool (* modified after simulation *) }

type result =
  { blocks_observed: int
  ; blocks_confirmed: int
  ; votes_observed: int
  ; votes_confirmed: int
  ; mean_interval: float
  ; blocks: sim_block list
  ; votes: sim_vote list
  ; pow_scale: float }

type 'a column = {title: string; f: 'a -> string}

let csv_head cols = String.concat "," (List.map (fun x -> x.title) cols)
let csv_row cols row = String.concat "," (List.map (fun x -> x.f row) cols)

type row = {p: params; r: result}

module ToString = struct
  let i = string_of_int
  let f = string_of_float
  let s = string_of_strategy
  let d = string_of_distribution
  let h = Link.to_string
  let b x = if x then "1" else "0"
end

let cols : row column list =
  let open ToString in
  [ {title= "protocol"; f= (fun x -> s x.p.protocol)}
  ; {title= "confirmations"; f= (fun x -> i x.p.confirmations)}
  ; {title= "quorum.size"; f= (fun x -> i x.p.quorum_size)}
  ; {title= "pow.scale"; f= (fun x -> f x.r.pow_scale)}
  ; {title= "n.blocks"; f= (fun x -> i x.p.n_blocks)}
  ; {title= "n.nodes"; f= (fun x -> i x.p.n_nodes)}
  ; {title= "alpha"; f= (fun x -> f x.p.alpha)}
  ; {title= "strategy"; f= (fun x -> s x.p.strategy)}
  ; {title= "delta.dist"; f= (fun x -> d x.p.delta_dist)}
  ; {title= "delta.vote"; f= (fun x -> f x.p.delta_vote)}
  ; {title= "delta.block"; f= (fun x -> f x.p.delta_block)}
  ; {title= "churn"; f= (fun x -> f x.p.churn)}
  ; {title= "churn.eclipse.time"; f= (fun x -> f x.p.eclipse_time)}
  ; {title= "leader.failure.rate"; f= (fun x -> f x.p.leader_failure_rate)}
  ; {title= "blocks.observed"; f= (fun x -> i x.r.blocks_observed)}
  ; {title= "blocks.confirmed"; f= (fun x -> i x.r.blocks_confirmed)}
  ; {title= "votes.observed"; f= (fun x -> i x.r.votes_observed)}
  ; {title= "votes.confirmed"; f= (fun x -> i x.r.votes_confirmed)}
  ; {title= "mean.interval"; f= (fun x -> f x.r.mean_interval)} ]

let block_cols : sim_block column list =
  let open ToString in
  [ {title= "hash"; f= (fun x -> h x.hash)}
  ; {title= "parent"; f= (fun x -> h x.data.parent)}
  ; {title= "published.at"; f= (fun x -> f x.published_at)}
  ; {title= "height"; f= (fun x -> i x.height)}
  ; {title= "confirmed"; f= (fun x -> b x.confirmed)} ]

type net_event =
  | Broadcast of {src: int; cnt: int; m: message}
  | Deliver of {src: int; rcv: int; cnt: int; m: message}

type event = ATV of {nth: int; node: int} | Net of net_event | Shutdown
type eclipse = {till: float; queue: net_event Queue.t}
type node = {m: (module Node); mutable eclipse: eclipse option}

let string_of_block (b : block) =
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

include struct
  open Event_queue

  (* Imperative interface for functional event queue *)
  class ['a] scheduler =
    object
      val mutable time = 0.

      val mutable queue : 'a Event_queue.t = empty

      method now = time

      method schedule ?(delay = 0.) event =
        queue <- Event_queue.schedule queue (time +. delay) event

      method next =
        let time', event, queue' = next queue in
        time <- time' ;
        queue <- queue' ;
        (time', event)

      method empty = queue = empty
    end
end

type state =
  { mutable height: int
  ; mutable pow_scale: float
  ; mutable atv_cnt: int
  ; mutable shutdown: bool
  ; attacker_id: DSA.public_key
  ; attacker_secret: DSA.private_key
  ; target_height: int
  ; nodes: node array
  ; scheduler: event scheduler
  ; blocks: sim_block BlockStore.t
        (* TODO this could be a (block Link.t, sim_block) Hashtbl.t *)
  ; votes: (vote, sim_vote) Hashtbl.t
  ; mutable head: block Link.t
  ; da_vote_cnts: float array
        (* count votes for last [p.da_window + p.confirmations] block heights *)
  }

let rec eclipse_random_node till nodes =
  (* Pick a node but not the attacker *)
  let i = Random.int (Array.length nodes - 1) + 1 in
  match nodes.(i).eclipse with
  | Some _ -> eclipse_random_node till nodes
  | None -> nodes.(i).eclipse <- Some {queue= Queue.create (); till}

(* Get time of next ATV and assign random ATV recipient. *)
let schedule_atv ~p ~s =
  let delay = draw Exponential s.pow_scale
  and node =
    if Random.float 1. <= p.alpha then 0
    else Random.int (Array.length s.nodes - 1) + 1 in
  s.scheduler#schedule ~delay (ATV {nth= s.atv_cnt; node})

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
              s.scheduler#schedule ~delay (Net (Deliver {src; rcv; cnt; m})))
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
          let now = s.scheduler#now in
          if now < eclipse.till then Queue.push ev eclipse.queue
          else (
            uneclipse n ;
            handle_net ev ;
            eclipse_random_node (now +. p.eclipse_time) s.nodes ) )

let quorum_threshold = Weight.max_weight

let broadcast =
  let message_cnt = ref 0 in
  fun (scheduler : event scheduler) nr ->
    let module B = struct
      let send m =
        incr message_cnt ;
        scheduler#schedule (Net (Broadcast {cnt= !message_cnt; m; src= nr}))
    end in
    (module B : Broadcast)

let spawn ~p scheduler id secret strategy =
  let module Config = struct
    let quorum_size = p.quorum_size
    let quorum_threshold = quorum_threshold
    let confirmations = p.confirmations
    let my_id = id
    let my_secret = secret
    let now () = scheduler#now
  end in
  let (module Broadcast) = broadcast scheduler DSA.(int_of_id id)
  and (module Implementation) = implementation_of_strategy strategy in
  (module Implementation.Spawn (Broadcast) (Config) : Node)

let ratio a b = float_of_int a /. float_of_int b

let result ~p ~s : result =
  let () =
    (* mark blocks on the longest chain as confirmed *)
    let ptr = ref s.head in
    let continue = ref true in
    while !continue do
      match BlockStore.get s.blocks !ptr with
      | None -> continue := false
      | Some b ->
          b.confirmed <- true ;
          ptr := b.data.parent ;
          (* mark votes confirmed *)
          List.iter
            (fun (id, sol) ->
              (Hashtbl.find s.votes (b.data.parent, id, sol)).confirmed <- true)
            b.data.quorum
    done in
  let blocks, last_publication =
    (* get all blocks up to confirmed height and sort by publication *)
    BlockStore.elements s.blocks
    |> List.filter (fun (a : sim_block) -> a.height <= p.n_blocks)
    |> List.sort (fun a b -> Float.compare b.published_at a.published_at)
    |> fun l -> (List.rev l, (List.hd l).published_at) in
  let votes =
    (* get all votes up to confirmed target height and sort by publication *)
    Hashtbl.fold
      (fun _v (sv : sim_vote) acc ->
        if sv.target_height <= p.n_blocks then sv :: acc else acc)
      s.votes []
    |> List.sort (fun (a : sim_vote) b ->
           Float.compare a.published_at b.published_at) in
  let blocks_confirmed =
    (* count confirmed blocks. Should equal requested number of blocks. *)
    let cnt = ref 0 in
    List.iter (fun b -> if b.confirmed then incr cnt) blocks ;
    assert (!cnt = p.n_blocks) ;
    !cnt
  and votes_confirmed =
    (* count confirmed votes. Should equal blocks_confirmed * quorum_size *)
    let cnt = ref 0 in
    List.iter (fun (v : sim_vote) -> if v.confirmed then incr cnt) votes ;
    assert (!cnt = p.n_blocks * p.quorum_size) ;
    !cnt in
  { blocks_confirmed
  ; blocks_observed= List.length blocks
  ; votes_confirmed
  ; votes_observed= List.length votes
  ; mean_interval= last_publication /. float_of_int blocks_confirmed
  ; blocks
  ; votes
  ; pow_scale= s.pow_scale }

let init ~p =
  let attacker_id, attacker_secret = DSA.id_of_int 0
  and scheduler = new scheduler in
  let nodes : node array =
    Array.init p.n_nodes (fun i ->
        let m =
          if i = 0 then
            spawn ~p scheduler attacker_id attacker_secret p.strategy
          else
            let id, secret = DSA.id_of_int i in
            spawn ~p scheduler id secret p.protocol in
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
  let da_vote_cnts =
    (* initialize the DA array as if target orphan rate would be actual orphan
       rate *)
    let orphan_rate = match p.orphan_rate with Some x -> x | None -> 0. in
    let should_observe = float_of_int p.quorum_size /. (1. -. orphan_rate) in
    Array.make (p.da_window + p.confirmations) should_observe in
  { height= 0
  ; atv_cnt= 0
  ; shutdown= false
  ; attacker_id
  ; attacker_secret
  ; target_height= p.n_blocks + p.confirmations
  ; nodes
  ; scheduler
  ; pow_scale= p.pow_scale
  ; da_vote_cnts
  ; blocks=
      BlockStore.create
        {parent= (fun (x : sim_block) -> x.data.parent); this= (fun x -> x.hash)}
  ; votes= Hashtbl.create (p.quorum_size * (p.n_blocks + p.confirmations))
  ; head= Link.hash Prot_parallel.genesis }

let event_filter verbosity = function
  | Net (Deliver _) when verbosity >= 3 -> true
  | ATV _ when verbosity >= 2 -> true
  | Net (Broadcast _) when verbosity >= 1 -> true
  | _ -> false

let height blockstore parent =
  match BlockStore.get blockstore parent with
  | Some (p : sim_block) -> Some (p.height + 1)
  | None when parent = Link.hash Prot_parallel.genesis ->
      (* genesis is not in this blockstore *)
      Some 1
  | None -> None

let track_vote ~s (v : vote) =
  if not (Hashtbl.mem s.votes v) then (
    let target_height =
      let parent, _, _ = v in
      match height s.blocks parent with
      | Some x -> x
      | None -> failwith "invalid parent in vote broadcast" in
    let sv =
      {data= v; published_at= s.scheduler#now; confirmed= false; target_height}
    and da_i = target_height mod Array.length s.da_vote_cnts in
    Hashtbl.add s.votes v sv ;
    s.da_vote_cnts.(da_i) <- s.da_vote_cnts.(da_i) +. 1. )

let adjust_difficulty ~p ~s =
  match p.orphan_rate with
  | None -> ()
  | Some r ->
      let observed = ref 0. in
      let () =
        let m = Array.length s.da_vote_cnts in
        let lower = s.height - p.confirmations - p.da_window + 1 + m
        and upper = s.height - p.confirmations + m in
        let () = s.da_vote_cnts.((s.height + 1) mod m) <- 0. in
        for height = lower to upper do
          observed := !observed +. s.da_vote_cnts.(height mod m)
        done in
      let observed = !observed /. float_of_int p.da_window
      and should_observe = float_of_int p.quorum_size /. (1. -. r) in
      s.pow_scale <- p.pow_scale *. should_observe /. observed

let track_block ~p ~s (b : block) =
  let hash = Link.hash b and parent = b.parent in
  if not (BlockStore.mem s.blocks hash) then (
    let height =
      match height s.blocks parent with
      | Some x -> x
      | None -> failwith "invalid parent in block broadcast" in
    let sb =
      {hash; data= b; height; published_at= s.scheduler#now; confirmed= false}
    in
    BlockStore.add s.blocks sb ;
    (* track votes in quorum *)
    List.iter (fun (id, sol) -> track_vote ~s (b.parent, id, sol)) b.quorum ;
    (* update head? *)
    if height > s.height then (
      s.height <- height ;
      s.head <- hash ;
      adjust_difficulty ~s ~p ) ;
    (* shutdown ? *)
    if s.height >= s.target_height && not s.shutdown then (
      s.shutdown <- true ;
      s.scheduler#schedule Shutdown ) )
  else Printf.eprintf "WARNING: hash collision observed?!"

let simulate io p =
  let s = init ~p in
  let log t e =
    if event_filter io.verbosity e then
      Printf.printf "%14.5f    %s\n%!" t (string_of_event e) in
  schedule_atv ~p ~s ;
  while not s.scheduler#empty do
    let t, e = s.scheduler#next in
    let () =
      match e with
      | Net (Broadcast {m= Block b; _}) -> track_block ~p ~s b
      | Net (Broadcast {m= Vote v; _}) -> track_vote ~s v
      | _ -> () in
    handle_event ~p ~s e ; log t e
  done ;
  result ~p ~s
