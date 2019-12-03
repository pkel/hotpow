let () = Random.self_init ()

open Simlib
open Primitives

module Params = struct
  [@@@ocaml.warning "-39"]

  type t =
    { n_blocks: int [@default 128] [@aka ["b"]]
          (** Set amount of blocks to simulate. *)
    ; quorum_size: int [@default 8] [@aka ["q"]]  (** Set the quorum size. *)
    ; topology: string [@aka ["t"]] [@default "-"]
          (** Specify topology (input) GraphML file.
              Use --topology '-' to read topology from STDIN. *)
    ; result: string [@aka ["r"]] [@default "-"]
          (** Specify result (outout) GraphML file.
              Use --result '-' to write result to STDOUT. *)
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
    ; verbosity: int [@aka ["v"]] [@default 0]
          (** Print events. > 0 : Message send; > 1 : ATV assignments;
              > 2 : Message delivery *)
    ; format: bool [@default false]  (** Break and indent XML output. *) }
  [@@deriving cmdliner]

  let check p =
    let fail p msg =
      Printf.eprintf "Invalid parameter --%s: %s\n%!" p msg ;
      exit 1 in
    if p.n_blocks < 1 then fail "n-blocks" "must be >= 1" ;
    if p.quorum_size < 1 then fail "quorum-size" "must be >= 1" ;
    if p.eclipse_time <= 0. then fail "eclipse-time" "must be > 0" ;
    if p.churn < 0. || p.churn > 1. then fail "churn" "must be in [0,1]" ;
    if p.leader_failure_rate < 0. || p.leader_failure_rate > 1. then
      fail "leader-failure-rate" "must be in [0,1]"
end

module Network = struct
  type address = int
  (** {1} Simulated Network

      Simulated network based on annotated {!Graph}.
  *)

  type msg = {src: address; rcv: address; m: message}

  type event =
    | Send of msg
    | Deliver of msg
    | Broadcast of {src: address; m: message}

  type eclipse = {till: float; queue: event Queue.t}

  type node_data =
    { alpha: float
    ; strategy: Strategy.t
    ; instance: (module Node)
    ; mutable eclipse: eclipse option
    ; mutable atv_count: int }

  type edge_data =
    { latency: float Rvar.t
    ; bandwidth: float Rvar.t
    ; mutable messages_transferred: int
    ; mutable blocks_transferred: int
    ; mutable votes_transferred: int }

  type t = (node_data, edge_data) Graph.t
  type node = node_data Graph.node
  type edge = (node_data, edge_data) Graph.edge

  let count_msg data msg =
    data.messages_transferred <- data.messages_transferred + 1 ;
    match msg with
    | Block _ -> data.blocks_transferred <- data.blocks_transferred + 1
    | Vote _ -> data.votes_transferred <- data.votes_transferred + 1

  let to_graphml =
    let open Graphml in
    let int i = Double (float_of_int i) in
    let rvar x = String (Rvar.float_to_string x) in
    let strategy x = String (Strategy.to_string x) in
    let e data =
      [ ("latency", rvar data.latency); ("bandwidth", rvar data.bandwidth)
      ; ("messages_transferred", int data.messages_transferred)
      ; ("blocks_transferred", int data.blocks_transferred)
      ; ("votes_transferred", int data.votes_transferred) ]
    and n data =
      [ ("alpha", Double data.alpha); ("strategy", strategy data.strategy)
      ; ("atv_count", int data.atv_count) ] in
    Graph.to_graphml ~n ~e

  let of_graphml
      ~(spawn : addr:address -> (module Implementation) -> (module Node)) =
    let open Graphml in
    let strf = Printf.sprintf in
    let float ~default key data =
      match get_double key data with
      | Ok d -> d
      | Error `Key_not_found -> default
      | Error `Type_mismatch ->
          failwith (strf "unexpected data type for field \"%s\"" key)
    and string ~default key data =
      match get_string key data with
      | Ok d -> d
      | Error `Key_not_found -> default
      | Error `Type_mismatch ->
          failwith (strf "unexpected data type for field \"%s\"" key)
    and int ~default key data =
      match get_double key data with
      | Ok d -> Float.to_int d
      | Error `Key_not_found -> default
      | Error `Type_mismatch ->
          failwith (strf "unexpected data type for field \"%s\"" key)
    and rvar ~default key data =
      match List.assoc_opt key data with
      | Some (String s) -> Rvar.(float_of_string s |> fail)
      | Some (Double d) -> Rvar.constant d
      | None -> Rvar.constant default
      | _ -> failwith (strf "unexpected data type for field \"%s\"" key) in
    let n ~id data =
      let strategy =
        string ~default:"naive" "strategy" data |> Strategy.of_string in
      { strategy
      ; instance= Strategy.to_implementation strategy |> spawn ~addr:id
      ; alpha= float ~default:1. "alpha" data
      ; eclipse= None
      ; atv_count= int ~default:0 "atv_count" data }
    and e data =
      { latency= rvar ~default:0. "latency" data
      ; bandwidth= rvar ~default:0. "bandwidth" data
      ; messages_transferred= int ~default:0 "messages_transferred" data
      ; blocks_transferred= int ~default:0 "blocks_transferred" data
      ; votes_transferred= int ~default:0 "votes_transferred" data } in
    Graph.of_graphml ~n ~e
end

module Event = struct
  type t =
    | ATV of {nth: int; node: Network.node}
    | Net of Network.event
    | Shutdown

  let to_string =
    let sprintf = Printf.sprintf in
    let open Graph in
    function
    | Net (Broadcast b) ->
        sprintf "n%i broadcasts %s" b.src (message_to_string b.m)
    | Net (Send m) ->
        sprintf "n%i sends %s to n%i" m.src (message_to_string m.m) m.rcv
    | Net (Deliver m) ->
        sprintf "n%i receives %s from n%i" m.rcv (message_to_string m.m) m.src
    | ATV {nth; node} -> sprintf "assign %ith ATV to n%i" nth node.id
    | Shutdown -> sprintf "stop simulation"

  module Queue = struct
    type event = t

    open Event_queue

    let eq : event t ref = ref empty
    let time = ref 0.
    let schedule ?(delay = 0.) event = eq := schedule !eq (!time +. delay) event

    let next () =
      let time', event, eq' = next !eq in
      time := time' ;
      eq := eq' ;
      (time', event)

    let is_empty () = !eq = empty
  end
end

type simulation =
  { (* (mutable) state of a simulation *)
    mutable height: int
  ; mutable shutdown: bool
  ; net: Network.t
  ; nodes: Network.node array
  ; mutable atv_count: int
  ; atv_rate: float
  ; atv_delta: float Rvar.t
  ; atv_receiver: Network.node Rvar.t }

type result =
  { (* stats derived from state after simulation *)
    block_cnt: int
  ; branches: int
  ; branch_depth: int
  ; max_vote: float
  ; max_vote_mean: float
  ; max_vote_sd: float
  ; efficiency: float
  ; block_time: float }

let graph_data ~r ~s ~(p : Params.t) : Graphml.data =
  let open Graphml in
  let i n = Double (float_of_int n) and f x = Double x and string x = String x in
  [ ("quorum_size", i p.quorum_size); ("churn_rate", f p.churn)
  ; ("churn_eclipse_time", f p.eclipse_time)
  ; ("leader_failure_rate", f p.leader_failure_rate)
  ; ("topology", string p.topology); ("n_nodes", i (Graph.cardinality s.net))
  ; ("n_branches", i r.branches); ("branch_depth", i r.branch_depth)
  ; ("n_blocks", i r.block_cnt); ("max_vote", f r.max_vote)
  ; ("max_vote_mean", f r.max_vote_mean); ("max_vote_sd", f r.max_vote_sd)
  ; ("efficiency", f r.efficiency); ("atv_count", i s.atv_count)
  ; ("block_time", f r.block_time) ]

let graph ~p ~r ~s =
  let g = Network.to_graphml s.net in
  {g with data= graph_data ~r ~s ~p @ g.data}

let rec eclipse_random_node till (nodes : Network.node array) =
  let open Network in
  let open Graph in
  (* TODO: exclude attacker nodes *)
  let i = Random.int (Array.length nodes - 1) + 1 in
  let data = nodes.(i).data in
  match data.eclipse with
  | Some _ -> eclipse_random_node till nodes
  | None -> data.eclipse <- Some {queue= Queue.create (); till}

(* Get time of next ATV and assign random ATV recipient. *)
let schedule_atv ~s () =
  let delay = Rvar.sample s.atv_delta and node = Rvar.sample s.atv_receiver in
  Event.Queue.schedule ~delay (ATV {nth= s.atv_count; node}) ;
  s.atv_count <- s.atv_count + 1 ;
  node.data.atv_count <- node.data.atv_count + 1

let transfer_time ~latency ~bandwidth =
  let open Rvar in
  function
  | Block _ ->
      (* inventory + request + block transfer time *)
      sample latency +. sample latency +. sample bandwidth
  | Vote _ ->
      (* Votes fit in one packet *)
      sample latency

let handle_event ~p ~s =
  let open Network in
  let flood ~src ~m =
    List.iter
      (fun (dst, ({latency; bandwidth; _} as data)) ->
        let msg = {src; rcv= dst; m} in
        Network.count_msg data m ;
        Event.(Queue.schedule (Net (Send msg))) ;
        let delay = transfer_time ~latency ~bandwidth m in
        Event.(Queue.schedule ~delay (Net (Deliver msg))))
      Graph.(out_edges s.net src) in
  let handle_net = function
    | Deliver {rcv; m; _} ->
        let n = s.nodes.(rcv) in
        let (module N) = n.data.instance in
        if N.on_receive m then flood ~m ~src:rcv
    | Send _ -> ()
    | Broadcast {m= Block _; src; _}
    (* TODO: make this rvar bernoulli *)
      when src > 0 && Random.float 1. <= Params.(p.leader_failure_rate) ->
        (* leader fails. *) ()
    | Broadcast {src; m} -> flood ~m ~src in
  let uneclipse (n : Network.node) =
    match Graph.(n.data.eclipse) with
    | None -> ()
    | Some eclipse ->
        Queue.iter handle_net eclipse.queue ;
        n.data.eclipse <- None in
  let schedule_atv = schedule_atv ~s in
  function
  | Event.ATV {node; nth} ->
      let (module N : Node) = node.data.instance in
      if not s.shutdown then schedule_atv () ;
      N.on_atv nth
  | Shutdown -> List.iter uneclipse (Graph.get_nodes s.net)
  | Net ev -> (
      let actor =
        match ev with
        | Broadcast {src; _} | Send {src; _} -> src
        | Deliver {rcv; _} -> rcv in
      let n = s.nodes.(actor) in
      match n.data.eclipse with
      | None -> handle_net ev
      | Some eclipse ->
          if !Event.Queue.time < eclipse.till then Queue.push ev eclipse.queue
          else (
            uneclipse n ;
            handle_net ev ;
            eclipse_random_node (!Event.Queue.time +. p.eclipse_time) s.nodes )
      )

let quorum_threshold = Weight.max_weight

let broadcast =
  let open Network in
  fun node_addr ->
    let module B = struct
      let send m = Event.Queue.schedule (Net (Broadcast {m; src= node_addr}))
    end in
    (module B : Broadcast)

let spawn ~p ~addr implementation =
  let open Params in
  let id, secret = DSA.generate_id () in
  let module Config = struct
    let quorum_size = p.quorum_size
    let quorum_threshold = quorum_threshold
    let my_id = id
    let my_secret = secret
  end in
  let (module Broadcast) = broadcast addr
  and (module Implementation : Implementation) = implementation in
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
  let open Graph in
  let open Network in
  let f (n : node) =
    let (module N : Node) = n.data.instance in
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

let from_uneclipsed get s =
  let open Graph in
  let open Network in
  let rec f seq =
    match seq () with
    | Seq.Nil -> get s.nodes.(0).data.instance
    | Cons ((n : node), seq) -> (
      match n.data.eclipse with None -> get n.data.instance | Some _ -> f seq )
  in
  f Array.(to_seq s.nodes)

let ratio a b = float_of_int a /. float_of_int b

let result ~p ~s =
  let open Params in
  let max_vote, max_vote_mean, max_vote_sd =
    from_uneclipsed max_vote_weight_stats s
  and block_time = !Event.Queue.time /. float_of_int (s.height + 3)
  and efficiency =
    ratio ((s.height + 3) * p.quorum_size) s.atv_count
    (* plus three because 3 blocks are not yet committed *) in
  let block_cnt = s.height
  and branches, branch_depth = branch_analysis s.nodes in
  { block_cnt
  ; branches
  ; branch_depth
  ; max_vote_mean
  ; max_vote_sd
  ; max_vote
  ; efficiency
  ; block_time }

let raise_str_err = function Ok x -> x | Error str -> failwith str

let init ~p graph =
  let open Params in
  let spawn = spawn ~p in
  let net = Network.of_graphml ~spawn graph in
  let n_nodes = Graph.cardinality net in
  let atv_rate = float_of_int p.quorum_size
  and nodes : Network.node array = Graph.get_nodes net |> Array.of_list in
  let atv_delta =
    match Rvar.exponential' ~rate:atv_rate with
    | `Ok a -> a
    | _ -> failwith "invalid atv rate"
  and atv_receiver =
    List.map
      (fun (n : Network.node) -> (Network.(Graph.(n.data.alpha)), n))
      (Graph.get_nodes net)
    |> Rvar.discrete
    |> function
    | `Ok v -> v | `Invalid_parameters s -> failwith ("topology: " ^ s) in
  let () =
    (* eclipse first set of nodes *)
    let n = int_of_float (floor (float_of_int n_nodes *. p.churn)) in
    let d = p.eclipse_time /. float_of_int n in
    let rec eclipse n =
      if n = 0 then ()
      else (
        eclipse_random_node (float_of_int n *. d) nodes ;
        eclipse (n - 1) ) in
    eclipse n in
  { height= 0
  ; atv_count= 0
  ; shutdown= false
  ; atv_rate
  ; atv_delta
  ; atv_receiver
  ; net
  ; nodes }

let event_filter verbosity = function
  | Event.Net (Deliver _) when verbosity >= 3 -> true
  | ATV _ when verbosity >= 2 -> true
  | Net (Broadcast _) when verbosity >= 1 -> true
  | _ -> false

let simulate ~p ~s =
  let log t e =
    if event_filter Params.(p.verbosity) e then
      Printf.printf "%14.5f    %s\n%!" t (Event.to_string e) in
  schedule_atv ~s () ;
  while not (Event.Queue.is_empty ()) do
    let t, e = Event.Queue.next () in
    handle_event ~p ~s e ;
    log t e ;
    match e with
    | Net (Broadcast {m= Block _; _}) ->
        s.height <- from_uneclipsed get_height s ;
        if s.height >= p.n_blocks then (
          s.shutdown <- true ;
          Event.(Queue.schedule Shutdown) )
    | _ -> ()
  done ;
  result ~p ~s

let term =
  let f p =
    Params.check p ;
    let graph =
      let c = if p.topology = "-" then stdin else open_in p.topology in
      let r =
        Ezxmlm.from_channel c |> snd |> Graphml.graph_of_xml |> raise_str_err
      in
      close_in c ; r in
    let s = init ~p graph in
    let r = simulate ~p ~s in
    let xml =
      let graph = Network.to_graphml s.net and data = graph_data ~r ~s ~p in
      Graphml.graph_to_xml {graph with data} |> raise_str_err
    and c = if p.result = "-" then stdout else open_out p.result
    and indent = if p.format then Some 2 else None in
    let out = Xmlm.make_output ~nl:true ~indent (`Channel c) in
    Ezxmlm.to_output out (None, xml) ;
    close_out c in
  Cmdliner.Term.(const f $ Params.cmdliner_term ())

let info = Cmdliner.(Term.info "sim" ~doc:"HotPow Simulator")
let () = Cmdliner.(Term.exit @@ Term.eval (term, info))
