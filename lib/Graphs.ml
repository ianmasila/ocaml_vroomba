open Util
open ReadingFiles
open Queues

(********************************************)
(*     Graphs via adjacency lists           *)
(********************************************)

module AdjacencyGraphs = struct
  type ('a, 'b) graph = {
    size : int;
    adj : int list array;
    node_payloads : (int * 'a) list ref;
    edge_labels : ((int * int) * 'b) list ref;
  }

  let mk_graph (n : int) : ('a, 'b) graph =
    { size = n; adj = Array.make n []; node_payloads = ref []; edge_labels = ref [] }

  let in_range (g : ('a, 'b) graph) (node : int) : bool = node >= 0 && node < g.size

  let set_payload (g : ('a, 'b) graph) (node : int) (p : 'a) : unit =
    assert (in_range g node);
    let pl = !(g.node_payloads) |> List.filter (fun (n, _) -> n <> node) in
    g.node_payloads := (node, p) :: pl

  (*******************************************)
  (*            Working with Edges           *)
  (*******************************************)

  let add_edge (g : ('a, 'b) graph) (src : int) (dst : int) : unit =
    assert (in_range g src && in_range g dst);
    let out = g.adj.(src) in
    let out' = List.filter (fun n -> n <> dst) out in
    g.adj.(src) <- dst :: out'

  let remove_edge (g : ('a, 'b) graph) (src : int) (dst : int) : unit =
    assert (in_range g src && in_range g dst);
    let out = g.adj.(src) in
    let out' = List.filter (fun n -> n <> dst) out in
    g.adj.(src) <- out'

  let set_edge_label (g : ('a, 'b) graph) (src : int) (dst : int) (l : 'b) : unit =
    assert (in_range g src && in_range g dst);
    let labs = !(g.edge_labels) in
    let labs' = List.filter (fun ((s, d), _) -> (s, d) <> (src, dst)) labs in
    g.edge_labels := ((src, dst), l) :: labs'

  (* Get all edges as pairs *)
  let edges (g : ('a, 'b) graph) : (int * int) list =
    let open DLLBasedQueue in
    let q = mk_queue g.size in
    for i = 0 to g.size - 1 do
      let next = g.adj.(i) in
      List.iter (fun n -> enqueue q (i, n)) next
    done;
    queue_to_list q

  (*******************************************)
  (*    Reading/writing simple graphs        *)
  (*******************************************)

  (* Reading the graph *)

  let adjacency_int_graph_of_strings (ls : string list) : (int, 'a) graph =
    let size = trimmer (List.hd ls) |> int_of_string in
    let g = mk_graph size in
    let edges = List.tl ls in
    let pairs =
      List.map (fun s -> trimmer s) edges
      |> List.filter (fun s -> String.length s > 0)
      |> List.map (fun s ->
             let splitted = splitter s in
             let src = int_of_string @@ List.hd splitted in
             let dst = int_of_string @@ List.hd @@ List.tl splitted in
             (src, dst))
    in
    for i = 0 to g.size - 1 do
      set_payload g i i
    done;
    List.iter (fun (s, d) -> add_edge g s d) pairs;
    g

  (* No payloads *)
  let read_simple_graph_shape_from_file (filename : string) : (int, 'a) graph =
    let ls = read_file_to_strings filename in
    adjacency_int_graph_of_strings ls

  let graph_to_string (g : ('a, 'b) graph) : string =
    let s0 = string_of_int g.size in
    let ls = List.map (fun (s, d) -> Printf.sprintf "%d %d" s d) (edges g) in
    String.concat "\n" (s0 :: ls)

  (* Dump graph to file *)
  let wirte_simple_graph_shape_to_file (filename : string) (g : ('a, 'b) graph) : unit =
    graph_to_string g |> write_string_to_file filename
end

(*******************************************)
(*    Rendering the graph via GraphViz     *)
(*******************************************)

let graphviz_string_of_graph
    (gtyp : string)
    (conn : string)
    (vattrib : int -> string)
    (eattrib : int * int -> string)
    (g : ('a, 'b) AdjacencyGraphs.graph)
    : string
  =
  let preamble = gtyp ^ " {\n" in
  let epilogue = "}" in
  let body =
    AdjacencyGraphs.edges g
    |> List.map (fun (s, d) ->
           Printf.sprintf "%s %s %s %s" (vattrib s) conn (vattrib d) (eattrib (s, d)))
    |> String.concat ";\n"
  in
  preamble ^ body ^ epilogue

let graphviz_no_payload (g : ('a, 'b) AdjacencyGraphs.graph) (out : string) : unit =
  let s = graphviz_string_of_graph "digraph" " -> " string_of_int (fun _ -> "") g in
  write_string_to_file out s

(*
Rendering via GraphViz:

dot -Tpdf filename.dot -o outfile.pdf

for instance:

dot -Tpdf simple.dot -o simple.pdf

*)

(*
Question: which operations are not so good for simple graphs?
*)

(**************************************************************)
(*        Mutable graphs as a linked data structures          *)
(**************************************************************)

module LinkedGraphs = struct
  (*************************************************)
  (*                     Nodes                     *)
  (*************************************************)

  type 'a node = {
    id : int;
    value : 'a ref;
    next : int list ref;
    prev : int list ref;
  }

  let get_value (n : 'a node) : 'a = !(n.value)

  let get_next (n : 'a node) : int list = !(n.next)

  let get_prev (n : 'a node) : int list = !(n.prev)

  let add_prev (node : 'a node) (src : int) : unit =
    let prev' = get_prev node |> List.filter (fun n -> n <> src) in
    node.prev := src :: prev'

  let add_next (node : 'a node) (dst : int) : unit =
    let next' = get_next node |> List.filter (fun n -> n <> dst) in
    node.next := dst :: next'

  (*************************************************)
  (*           Auxiliary definitions               *)
  (*************************************************)

  open BST
  open BetterHashTable
  module Set = BinarySearchTree

  module NodeTable = ResizableListBasedHashTable (struct
    type t = int
  end)

  module EdgeTable = ResizableListBasedHashTable (struct
    type t = int * int
  end)

  type 'a set = 'a Set.tree

  (*************************************************)
  (*                Working with Graphs            *)
  (*************************************************)

  type ('a, 'b) graph = {
    next_node_id : int ref;
    nodes : int set;
    node_map : (int * 'a node) NodeTable.hash_table;
    edges : (int * int) set;
    edge_labels : ((int * int) * 'b) EdgeTable.hash_table;
  }

  (**************************************************)
  (*                Querying the graph              *)
  (**************************************************)

  (* Graph size *)
  let v_size (g : ('a, 'b) graph) : int = !(g.next_node_id)

  let e_size (g : ('a, 'b) graph) : int = BinarySearchTree.get_size g.edges

  let get_nodes (g : ('a, 'b) graph) : int list = Set.elements g.nodes

  (* Refer to the node in the graph *)
  let get_node (g : ('a, 'b) graph) (i : int) : 'a node =
    get_exn @@ NodeTable.get g.node_map i

  let get_succ (g : ('a, 'b) graph) (n : int) : int list =
    let node = get_node g n in
    get_next node

  let get_prev (g : ('a, 'b) graph) (n : int) : int list =
    let node = get_node g n in
    get_prev node

  let node_in_graph (g : ('a, 'b) graph) (n : int) : bool =
    let nodes = g.nodes in
    Set.search nodes n <> None

  let edge_in_graph (g : ('a, 'b) graph) (src : int) (dst : int) : bool =
    let nodes = g.edges in
    Set.search nodes (src, dst) <> None

  (**************************************************)
  (*                Altering the graph              *)
  (**************************************************)

  (* Making a graph *)
  let mk_graph (() : unit) : ('a, 'b) graph =
    {
      next_node_id = ref 0;
      nodes = Set.mk_tree ();
      node_map = NodeTable.mk_new_table 10;
      edges = Set.mk_tree ();
      edge_labels = EdgeTable.mk_new_table 10;
    }

  (* Adding nodes *)
  let add_node (g : ('a, 'b) graph) (v : 'a) : unit =
    let new_id = !(g.next_node_id) in
    g.next_node_id := !(g.next_node_id) + 1;
    let node = { id = new_id; value = ref v; next = ref []; prev = ref [] } in
    (* Register node *)
    let _ = Set.insert g.nodes new_id in
    (* Register node payload *)
    NodeTable.insert g.node_map new_id node

  (* Adding edges *)
  let add_edge (g : ('a, 'b) graph) (src : int) (dst : int) : unit =
    assert (node_in_graph g src && node_in_graph g src);
    (* Register edge *)
    let _ = Set.insert g.edges (src, dst) in
    (* Add information to individual nodes *)
    let src_node = get_exn @@ NodeTable.get g.node_map src in
    let dst_node = get_exn @@ NodeTable.get g.node_map dst in
    add_prev dst_node src;
    add_next src_node dst

  let set_edge_label (g : ('a, 'b) graph) (src : int) (dst : int) (l : 'b) : unit =
    assert (node_in_graph g src && node_in_graph g src);
    assert (edge_in_graph g src dst);
    (* Register label *)
    EdgeTable.insert g.edge_labels (src, dst) l

  (*************************************************)
  (*        Switching between representations      *)
  (*************************************************)

  (* No node payloads *)
  let from_simple_adjacency_graph (ag : ('a, 'b) AdjacencyGraphs.graph) : ('a, 'b) graph =
    let g = mk_graph () in
    (* Add nodes *)
    for i = 0 to ag.size - 1 do
      let v = snd @@ List.find (fun (n, _) -> n = i) !(ag.node_payloads) in
      add_node g v
    done;
    (* Add edges *)
    for i = 0 to ag.size - 1 do
      ag.adj.(i)
      |> List.map (fun n -> (i, n))
      |> List.iter (fun (src, dst) -> add_edge g src dst)
    done;
    (* Add edge labels *)
    List.iter (fun ((src, dst), l) -> set_edge_label g src dst l) !(ag.edge_labels);
    g

  let to_adjacency_graph (g : ('a, 'b) graph) : ('a, 'b) AdjacencyGraphs.graph =
    let size = v_size g in
    let ag = AdjacencyGraphs.mk_graph size in
    (* Set node payloads *)
    Set.elements g.nodes
    |> List.iter (fun n ->
           let node = get_exn @@ NodeTable.get g.node_map n in
           AdjacencyGraphs.set_payload ag n (get_value node));
    (* Add edges *)
    let edges = Set.elements g.edges in
    List.iter (fun (src, dst) -> AdjacencyGraphs.add_edge ag src dst) edges;
    (* Add edges labels *)
    List.iter
      (fun (s, d) ->
        match EdgeTable.get g.edge_labels (s, d) with
        | None -> ()
        | Some l -> AdjacencyGraphs.set_edge_label ag s d l)
      edges;
    ag

  let parse_linked_int_graph (ls : string list) : (int, 'a) graph =
    AdjacencyGraphs.adjacency_int_graph_of_strings ls |> from_simple_adjacency_graph

  let read_simple_linked_graph_from_file (filename : string) : (int, 'a) graph =
    let ag = AdjacencyGraphs.read_simple_graph_shape_from_file filename in
    from_simple_adjacency_graph ag
end

(*************************************************)
(*         Reasoning about graphs                *)
(*************************************************)

let same_shape
    (ag1 : ('a, 'b) AdjacencyGraphs.graph)
    (ag2 : ('a, 'b) AdjacencyGraphs.graph)
    : bool
  =
  assert (ag1.size = ag2.size);
  let n = ag1.size in
  let comp x y = if x < y then -1 else if x > y then 1 else 0 in
  for i = 0 to n - 1 do
    let adj1 = ag1.adj.(i) |> List.sort comp in
    let adj2 = ag1.adj.(i) |> List.sort comp in
    assert (adj1 = adj2)
  done;
  true

(*************************************************)
(*           Some example graphs                 *)
(*************************************************)

let small_graph_shape : string list =
  ["6"; "0 1"; "0 3"; "3 1"; "1 4"; "1 2"; "2 4"; "2 5"; "5 1"; "5 5"]

let medium_graph_shape : string list =
  [
    "13";
    "0 1";
    "0 6";
    "0 5";
    "2 0";
    "2 3";
    "3 5";
    "5 4";
    "6 4";
    "7 6";
    "8 7";
    "6 9";
    "9 10";
    "9 11";
    "9 12";
    "11 12";
  ]
