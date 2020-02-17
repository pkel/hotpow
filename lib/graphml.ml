type graphtype = Directed | Undirected [@@deriving show {with_path= false}]

type value = String of string | Double of float | Boolean of bool
[@@deriving show {with_path= false}]

type data = (string * value) list [@@deriving show {with_path= false}]

type edge = {src: int; dst: int; data: data}
[@@deriving show {with_path= false}]

type node = {id: int; data: data} [@@deriving show {with_path= false}]

type graph = {nodes: node list; edges: edge list; type_: graphtype; data: data}
[@@deriving show {with_path= false}]

let graph_to_xml =
  let open Xmlm in
  let node_id i = Printf.sprintf "n%i" i
  and el ?(a = []) tag l : _ frag =
    `El ((("", tag), List.map (fun (k, v) -> (("", k), v)) a), l) in
  let data ht eon (key, d) =
    let s, t =
      match (d : value) with
      | String s -> (s, `String)
      | Double f -> (string_of_float f, `Double)
      | Boolean true -> ("true", `Boolean)
      | Boolean false -> ("false", `Boolean)
    and key' =
      match eon with
      | `Edge -> "e_" ^ key
      | `Node -> "v_" ^ key
      | `Graph -> "g_" ^ key in
    let () =
      match Hashtbl.find_opt ht key' with
      | None -> Hashtbl.add ht key' (t, eon, key)
      | Some (t', _, _) when t' = t -> ()
      | Some _ -> failwith (Printf.sprintf "conflicting types for key %s" key)
    in
    el "data" ~a:[("key", key')] [`Data s] in
  let nodes_edges_data_keys g =
    let keys = Hashtbl.create 7 in
    let edges =
      List.fold_left
        (fun edges e ->
          el "edge"
            ~a:[("source", node_id e.src); ("target", node_id e.dst)]
            (List.fold_left (fun acc d -> data keys `Edge d :: acc) [] e.data)
          :: edges)
        [] g.edges
    and nodes =
      List.fold_left
        (fun nodes n ->
          el "node"
            ~a:[("id", node_id n.id)]
            (List.fold_left (fun acc d -> data keys `Node d :: acc) [] n.data)
          :: nodes)
        [] g.nodes
    and data =
      List.fold_left (fun acc d -> data keys `Graph d :: acc) [] g.data in
    let keys =
      Hashtbl.fold
        (fun key (t, eon, name) acc ->
          let for_ =
            match eon with
            | `Edge -> "edge"
            | `Node -> "node"
            | `Graph -> "graph"
          and type_ =
            match t with
            | `Boolean -> "boolean"
            | `String -> "string"
            | `Double -> "double" in
          el "key"
            ~a:
              [ ("id", key); ("for", for_); ("attr.name", name)
              ; ("attr.type", type_) ]
            []
          :: acc)
        keys [] in
    (nodes, edges, data, keys) in
  fun g ->
    match nodes_edges_data_keys g with
    | exception Failure s -> Stdlib.Result.error s
    | nodes, edges, data, keys ->
        let edgedefault =
          match g.type_ with
          | Directed -> "directed"
          | Undirected -> "undirected" in
        Ok
          (el "graphml"
             ~a:
               [ ("xmlns", "http://graphml.graphdrawing.org/xmlns")
               ; ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
               ; ( "xsi:schemaLocation"
                 , "http://graphml.graphdrawing.org/xmlns \
                    http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd" ) ]
             ( keys
             @ [ el "graph"
                   ~a:[("edgedefault", edgedefault)]
                   (List.rev data @ List.rev nodes @ List.rev edges) ] ))

let graph_of_xml =
  let open Ezxmlm in
  let keys graph =
    let ht = Hashtbl.create 7 in
    List.iter
      (fun (attrs, _) ->
        let key =
          match get_attr "id" attrs with
          | s -> s
          | exception Not_found -> failwith "missing id attribute on key"
        and for_ =
          match get_attr "for" attrs with
          | "node" -> `Node
          | "edge" -> `Edge
          | "graph" -> `Graph
          | _ -> failwith "unexpected value of \"for\" attribute on key"
          | exception Not_found -> failwith "missing \"for\" attribute on key"
        and typ =
          match get_attr "attr.type" attrs with
          | "string" -> `String
          | "double" -> `Double
          | "boolean" -> `Boolean
          | _ -> failwith "unexpected value of \"attr.type\" attribute on key"
          | exception Not_found ->
              failwith "missing \"attr.type\" attribute on key"
        and name =
          match get_attr "attr.name" attrs with
          | s -> s
          | exception Not_found ->
              failwith "missing \"attr.name\" attribute on key" in
        Hashtbl.add ht (for_, key) (typ, name))
      (members_with_attr "key" graph) ;
    ht in
  let graph xml =
    let gml =
      match member "graphml" xml with
      | x -> x
      | exception Not_found -> failwith "invalid graphml file" in
    let graph, ed =
      match member_with_attr "graph" gml with
      | exception Not_found -> failwith "invalid graphml file"
      | attrs, childs ->
          let ed =
            match get_attr "edgedefault" attrs with
            | exception Not_found -> Undirected
            | "undirected" -> Undirected
            | "directed" -> Directed
            | _ ->
                failwith "unknown value for \"edgedefault\" attribute of graph"
          in
          (childs, ed) in
    (graph, keys gml, ed)
  and parse typ s =
    match typ with
    | `Boolean -> (
      match bool_of_string_opt s with
      | Some b -> Boolean b
      | None -> failwith "invalid boolean" )
    | `Double -> (
      match float_of_string_opt s with
      | Some f -> Double f
      | None -> failwith "invalid double" )
    | `String -> String s in
  let data keys eon frags =
    List.fold_left
      (fun data (attrs, childs) ->
        let key =
          try get_attr "key" attrs
          with Not_found -> failwith "missing \"key\" attribute on data" in
        let typ, name =
          match Hashtbl.find_opt keys (eon, key) with
          | None -> failwith ("unknown key: " ^ key)
          | Some x -> x
        and str =
          match childs with
          | [`Data s] -> s
          | _ -> failwith "non-data within data tag" in
        (name, parse typ str) :: data)
      []
      (members_with_attr "data" frags)
    |> List.rev
  and get_id key attrs =
    match get_attr key attrs with
    | s ->
        let parse_int s =
          try int_of_string s with Failure _ -> failwith "invalid id" in
        (* igraph exports integer ids extended with leading n *)
        let id = String.sub s 1 (String.length s - 1) |> parse_int in
        if id < 0 then failwith "negative node id" ;
        id
    | exception Not_found -> failwith ("missing " ^ key) in
  let nodes keys graph =
    List.fold_left
      (fun nodes (attrs, childs) ->
        {id= get_id "id" attrs; data= data keys `Node childs} :: nodes)
      []
      (members_with_attr "node" graph)
    |> List.rev
  and edges keys graph =
    List.fold_left
      (fun nodes (attrs, childs) ->
        { src= get_id "source" attrs
        ; dst= get_id "target" attrs
        ; data= data keys `Edge childs }
        :: nodes)
      []
      (members_with_attr "edge" graph)
    |> List.rev in
  fun xml ->
    try
      let graph, keys, type_ = graph xml in
      let data = data keys `Graph graph in
      let nodes = nodes keys graph in
      let edges = edges keys graph in
      Ok {nodes; edges; data; type_}
    with Failure s -> Error s

type 'a get_result = ('a, [`Key_not_found | `Type_mismatch]) result

let string = function String s -> Ok s | _ -> Error `Type_mismatch
let double = function Double d -> Ok d | _ -> Error `Type_mismatch
let boolean = function Boolean b -> Ok b | _ -> Error `Type_mismatch

let get f str data =
  match List.assoc_opt str data with
  | None -> Error `Key_not_found
  | Some d -> f d

let get_string = get string
let get_double = get double
let get_boolean = get boolean

type 'a get_result' = ('a * data, [`Key_not_found | `Type_mismatch]) result

let get' get str data =
  match get str data with
  | Ok x -> Ok (x, List.remove_assoc str data)
  | Error e -> Error e

let get_string' = get' get_string
let get_double' = get' get_double
let get_boolean' = get' get_boolean
