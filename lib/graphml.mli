(** {1} GraphML

    Writing and reading GraphML files with the goal of being compatible with
    iGraph.

*)

type graphtype = Directed | Undirected
type value = String of string | Double of float | Boolean of bool
type data = (string * value) list
type edge = {src: int; dst: int; data: data}
type node = {id: int; data: data}
type graph = {nodes: node list; edges: edge list; type_: graphtype; data: data}

val show_graph : graph -> string
val pp_graph : Format.formatter -> graph -> unit
val graph_to_xml : graph -> (Ezxmlm.node, string) result
val graph_of_xml : Ezxmlm.nodes -> (graph, string) result

(** {2} Data Access *)

type 'a get_result = ('a, [`Key_not_found | `Type_mismatch]) result

val get_string : string -> data -> string get_result
val get_double : string -> data -> float get_result
val get_boolean : string -> data -> bool get_result

type 'a get_result' = ('a * data, [`Key_not_found | `Type_mismatch]) result
(** data w/o the found field *)

val get_string' : string -> data -> string get_result'
val get_double' : string -> data -> float get_result'
val get_boolean' : string -> data -> bool get_result'
