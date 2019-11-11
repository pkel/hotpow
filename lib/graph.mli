(** {1} Annotated directed Graph *)

type ('n, 'e) t
type 'n node = {id: int; data: 'n}
type ('n, 'e) edge = {src: 'n node; dst: 'n node; data: 'e}

(* TODO: data -> edge_data/node_data; (add graph_data?) *)

val cardinality : ('n, 'e) t -> int
val get_nodes : ('n, 'e) t -> 'n node list
val get_edges : ('n, 'e) t -> ('n, 'e) edge list
val out_edges : ('n, 'e) t -> int -> (int * 'e) list
val create : ('n node * (int * 'e) list) list -> ('n, 'e) t

val of_graphml :
     e:(Graphml.data -> 'e)
  -> n:(id:int -> Graphml.data -> 'n)
  -> Graphml.graph
  -> ('n, 'e) t

val to_graphml :
     e:('e -> Graphml.data)
  -> n:('n -> Graphml.data)
  -> ('n, 'e) t
  -> Graphml.graph
