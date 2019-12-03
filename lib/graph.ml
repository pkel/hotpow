type 'n node = {id: int; data: 'n}
type ('n, 'e) edge = {src: 'n node; dst: 'n node; data: 'e}
type ('n, 'e) inode = {node: 'n node; links: (int * 'e) list}
type ('n, 'e) t = ('n, 'e) inode array

let get_node g i = g.(i).node
let cardinality = Array.length
let get_nodes g = Array.map (fun inode -> inode.node) g |> Array.to_list

let get_edges g =
  let node = get_node g in
  let _, edges =
    Array.fold_left
      (fun (src, edges) inode ->
        ( src + 1
        , List.fold_left
            (fun edges (dst, data) ->
              {src= node src; dst= node dst; data} :: edges)
            edges inode.links ))
      (0, []) g in
  edges

let out_edges g n =
  let src = g.(n) in
  List.fold_left (fun edges (dst, data) -> (dst, data) :: edges) [] src.links

let create x = List.map (fun (node, links) -> {node; links}) x |> Array.of_list

let to_graphml ~e ~n g =
  let open Graphml in
  let nodes, edges =
    Array.fold_left
      (fun (nodes, edges) inode ->
        ( {id= inode.node.id; data= n inode.node.data} :: nodes
        , List.fold_left
            (fun edges (dst, data) ->
              {src= inode.node.id; dst; data= e data} :: edges)
            edges inode.links ))
      ([], []) g in
  {nodes; edges; data= []; type_= Directed}

let of_graphml ~e ~n gml =
  if Graphml.(gml.type_ <> Directed) then failwith "need directed graph" ;
  let max_id, nodes =
    List.fold_left
      (fun (max_id, nodes) ({id; data} : Graphml.node) ->
        (max max_id id, {id; data= n ~id data} :: nodes))
      (-1, []) gml.nodes in
  if max_id < 0 then failwith "no nodes specified?" ;
  let g =
    let arr = Array.make (max_id + 1) None in
    List.iter (fun node -> arr.(node.id) <- Some {node; links= []}) nodes ;
    Array.mapi
      (fun id -> function Some x -> x
        | None -> {node= {id; data= n ~id []}; links= []})
      arr in
  List.iter
    (fun (edge : Graphml.edge) ->
      if edge.src > max_id then failwith "invalid source node id (too big)" ;
      if edge.dst > max_id then failwith "invalid target node id (too big)" ;
      if edge.src = edge.dst then failwith "loop (source = destination)" ;
      let inode = g.(edge.src) in
      g.(edge.src) <- {inode with links= (edge.dst, e edge.data) :: inode.links})
    gml.edges ;
  g
