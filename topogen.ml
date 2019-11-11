open Simlib

include struct
  [@@@ocaml.warning "-39"]

  let rvar_conv =
    let parse str =
      match Rvar.float_of_string str with
      | `Ok a -> Ok a
      | `Invalid_parameters msg -> Error (`Msg msg)
      | `Parse_error msg -> Error (`Msg msg)
    and print fmt t = Format.fprintf fmt "%s" (Rvar.float_to_string t) in
    (parse, print)

  (* Cmdliner PPX is not yet prepared for this: *)
  (* Cmdliner.Arg.conv ~docv:"DISTRIBUTION" (parse, print) *)

  let default_latency = Rvar.(exponential ~ev:0.00001 |> fail)

  (* 60 ms *)

  let default_bandwidth = Rvar.(uniform ~lower:0.001 ~upper:0.002 |> fail)

  (* 0.6s - 1.2s *)

  let default_alpha = Rvar.(exponential ~ev:1. |> fail)

  type params =
    { nodes: int [@default 32] [@aka ["n"]]  (** Set number of nodes. *)
    ; clique: bool [@default false]
          (** Generate fully interconnected graph. *)
    ; alpha: float Rvar.t
          [@default default_alpha] [@conv rvar_conv] [@aka ["a"]]
          (** Set distribution of computational power. *)
    ; bandwidth: float Rvar.t
          [@default default_bandwidth] [@conv rvar_conv] [@aka ["b"]]
          (** Set bandwidth distribution. *)
    ; latency: float Rvar.t
          [@default default_latency] [@conv rvar_conv] [@aka ["l"]]
          (** Set latency distribution. *)
    ; out_degree: int
          [@default 8]
          [@aka ["d"]]
          (* Set number of outgoing connections of each peers. Only applies if
             not generating a clique. *)
    ; format: bool [@default false]  (** Break and indent XML output. *) }
  [@@deriving cmdliner]
end

module Topo = struct
  type node_data = {alpha: float; strategy: Strategy.t}

  and edge_data = {latency: float Rvar.t; bandwidth: float Rvar.t}

  type t = (node_data, edge_data) Graph.t

  let to_graphml =
    let open Graphml in
    let rvar x = String (Rvar.float_to_string x) in
    let strategy x = String (Strategy.to_string x) in
    let e data =
      [("latency", rvar data.latency); ("bandwidth", rvar data.latency)]
    and n data =
      [("alpha", Double data.alpha); ("strategy", strategy data.strategy)]
    in
    Graph.to_graphml ~n ~e

  let clique ~alpha ~latency ~bandwidth n =
    if n < 1 then failwith "n < 1" ;
    let nodes =
      List.init n (fun self ->
          let node =
            Graph.{id= self; data= {alpha= Rvar.sample alpha; strategy= Naive}}
          and peers =
            List.init n (fun dst ->
                ( dst
                , { latency= Rvar.sample latency
                  ; bandwidth= Rvar.sample bandwidth } ) )
            |> List.filter (fun (dst, _) -> dst <> self)
          in
          (node, peers) )
    in
    Graph.create nodes

  let random ~alpha ~latency ~bandwidth ~d n =
    if n < 1 then failwith "n < 1" ;
    if d >= n then failwith "d >= n" ;
    let node self =
      let all = Array.make n false in
      let i = ref 1 in
      let () =
        all.(self) <- true ;
        while !i < d do
          let j = Random.int n in
          if not all.(j) then ( all.(j) <- true ; incr i )
        done ;
        all.(self) <- false
      in
      let peers =
        Array.to_seqi all
        |> Seq.filter_map (fun (dst, b) ->
               if b then
                 Some
                   ( dst
                   , { latency= Rvar.sample latency
                     ; bandwidth= Rvar.sample bandwidth } )
               else None )
        |> List.of_seq
      in
      ( Graph.{id= self; data= {alpha= Rvar.sample alpha; strategy= Naive}}
      , peers )
    in
    List.init n node |> Graph.create
end

let check_params p =
  let fail p msg =
    Printf.eprintf "Invalid parameter --%s: %s\n%!" p msg ;
    exit 1
  in
  if p.nodes < 1 then fail "nodes" "must be >= 1" ;
  if p.out_degree < 1 then fail "out-degree" "must be >= 1" ;
  if p.out_degree >= p.nodes then fail "out-degree" "must be < p.nodes"

let main ({latency; bandwidth; nodes; alpha; _} as p) =
  check_params p ;
  let indent = if p.format then Some 2 else None in
  let topo =
    let latency = Rvar.constant latency
    and bandwidth = Rvar.constant bandwidth in
    if p.clique then Topo.clique ~alpha ~latency ~bandwidth nodes
    else Topo.random ~alpha ~latency ~bandwidth nodes ~d:p.out_degree
  in
  Topo.to_graphml topo |> Graphml.graph_to_xml
  |> (function Ok x -> x | Error s -> failwith s)
  |> fun g ->
  Ezxmlm.to_output
    (Xmlm.make_output ~nl:true ~indent (`Channel stdout))
    (None, g)

let term = Cmdliner.Term.(const main $ params_cmdliner_term ())
let info = Cmdliner.(Term.info "topo" ~doc:"Topology Generator")
let () = Cmdliner.(Term.exit @@ Term.eval (term, info))
