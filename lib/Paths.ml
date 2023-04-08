open Util
open ArrayUtil
open ReadingFiles
open BST
open BinarySearchTree
open Graphs
open Reachability

(***********************************)
(*       Rendering digraphs        *)
(***********************************)

let get_ag_node_payload (ag : ('a, 'b) AdjacencyGraphs.graph) (n : int) : 'a =
  let open AdjacencyGraphs in
  List.find (fun (x, _) -> x = n) !(ag.node_payloads) |> snd

let get_ag_edge_label (ag : ('a, 'b) AdjacencyGraphs.graph) s d =
  let open AdjacencyGraphs in
  List.find (fun ((x, y), _) -> s = x && y = d) !(ag.edge_labels) |> snd

let get_linked_edge_label (g : ('a, 'b) LinkedGraphs.graph) (s : int) (d : int) : 'b =
  let open LinkedGraphs in
  EdgeTable.get g.edge_labels (s, d) |> get_exn

let get_linked_node_payload (g : ('a, 'b) LinkedGraphs.graph) (n : int) : 'a =
  let open LinkedGraphs in
  let node = NodeTable.get g.node_map n |> get_exn in
  !(node.value)

let graphviz_with_weights (g : (string, int) LinkedGraphs.graph) (out : string) : unit =
  let ag = LinkedGraphs.to_adjacency_graph g in
  let vattrib = get_ag_node_payload ag in
  let eattrib (s, d) =
    let l = get_ag_edge_label ag s d |> string_of_int in
    Printf.sprintf "[label=\"%s\", weight=\"%s\"]" l l
  in
  let s = graphviz_string_of_graph "digraph" " -> " vattrib eattrib ag in
  write_string_to_file out s

(***********************************)
(*            Distance             *)
(***********************************)

module Distance = struct
  type dist =
    | Finite of int
    | Infinity

  let ( < ) (d1 : dist) (d2 : dist) : bool =
    match (d1, d2) with
    | Infinity, _ -> false
    | Finite _, Infinity -> true
    | Finite x, Finite y -> x < y

  let ( <= ) (d1 : dist) (d2 : dist) : bool = d1 < d2 || d1 = d2

  let ( > ) (d1 : dist) (d2 : dist) : bool = not (d1 <= d2)

  let ( >= ) (d1 : dist) (d2 : dist) : bool = not (d1 < d2)

  let ( + ) (d1 : dist) (d2 : dist) : dist =
    match (d1, d2) with
    | Infinity, _ -> Infinity
    | _, Infinity -> Infinity
    | Finite x, Finite y -> Finite (x + y)

  let int_of_dist (d : dist) : int =
    match d with
    | Infinity -> raise (Failure "Cannot convert infinity to integer!")
    | Finite n -> n
end

(***********************************)
(*       Main operations for SSP   *)
(***********************************)

open LinkedGraphs

(* Initialise single source *)
let initialise_single_source (g : ('a, 'b) graph) (s : int)
    : (int -> int -> 'b)
      * (int * Distance.dist) NodeTable.hash_table
      * (int * 'c) NodeTable.hash_table
  =
  let open NodeTable in
  let open Distance in
  let n = v_size g in
  let dist_table = mk_new_table n in
  let prev_tree = mk_new_table n in
  for i = 0 to n - 1 do
    insert dist_table i Infinity
  done;
  insert dist_table s (Finite 0);
  let w = get_linked_edge_label g in
  (w, dist_table, prev_tree)

(* Get distance from the table *)
let dist (dist_table : (int * 'a) NodeTable.hash_table) (u : int) : 'a =
  get_exn @@ NodeTable.get dist_table u

(* Relax the distance between u and v *)
let relax
    (dist_table : (int * Distance.dist) NodeTable.hash_table)
    (prev_tree : (int * int) NodeTable.hash_table)
    (w : int -> int -> int)
    (u : int)
    (v : int)
    : unit
  =
  let open NodeTable in
  let open Distance in
  let vud = dist dist_table u + Finite (w u v) in
  if dist dist_table v > vud
  then (
    insert dist_table v vud;
    insert prev_tree v u)

(***********************************)
(*     Bellman-Ford algorithm      *)
(***********************************)

let bellman_ford (g : ('a, int) graph) (s : int)
    : ((int * int) NodeTable.hash_table * (int * Distance.dist) NodeTable.hash_table)
      * bool
  =
  let open Distance in
  let w, d, p = initialise_single_source g s in
  let all_edges = elements g.edges in
  for _ = 0 to v_size g - 1 do
    List.iter (fun (u, v) -> relax d p w u v) all_edges
  done;
  (* Check for negative cycles *)
  let rec check_neg_cycles es =
    match es with
    | [] -> true
    | (u, v) :: t ->
      if dist d v > dist d u + Finite (w u v) then false else check_neg_cycles t
  in
  ((p, d), check_neg_cycles all_edges)

(*

Question: Why check_neg_cycles works?

Question: What is a complexity of bellman_ford in terms of G.V and G.E?

*)

(***********************************)
(*       Dijkstra algorithm        *)
(***********************************)

(* Extract minimal distance in O(|remaining|) *)
let extract_min_dist
    (dist_table : (int * Distance.dist) NodeTable.hash_table)
    (remaining : int list ref)
    : int option
  =
  let open Distance in
  let res = ref None in
  let d = ref Infinity in
  List.iter
    (fun i ->
      let di = dist dist_table i in
      if di <= !d
      then (
        res := Some i;
        d := di))
    !remaining;
  match !res with
  | None -> None
  | Some i ->
    remaining := List.filter (fun j -> i <> j) !remaining;
    !res

let dijkstra (g : ('a, int) graph) (s : int)
    : (int * int) NodeTable.hash_table * (int * Distance.dist) NodeTable.hash_table
  =
  let w, d, p = initialise_single_source g s in
  (* Make queue of remaining uninspected nodes *)
  let q = ref (iota (v_size g - 1)) in
  while !q <> [] do
    let u = extract_min_dist d q |> get_exn in
    let adj = get_succ g u in
    List.iter (fun v -> relax d p w u v) adj
  done;
  (p, d)

(*

Question: Why Dijkstra relies on non-negative paths?

Hint: Think about local and global optimality.

*)

(***********************************)
(*   Renderring Graphs with Paths  *)
(***********************************)

let graphviz_with_min_paths
    (path_calculuator : (string, int) graph -> 'a -> (int * int) NodeTable.hash_table)
    (g : (string, int) graph)
    (s : 'a)
    (out : string)
    : unit
  =
  let p = path_calculuator g s in
  let attrib (u, v) =
    let l = get_linked_edge_label g u v |> string_of_int in
    match NodeTable.get p v with
    | Some z when u = z -> Printf.sprintf "[label=\"%s\", color=red,penwidth=3.0]" l
    | _ -> Printf.sprintf "[label=\"%s\"]" l
  in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s =
    graphviz_string_of_graph "digraph" " -> " (get_linked_node_payload g) attrib ag
  in
  write_string_to_file out s

(* Render graph with Bellman-Ford *)

let graphviz_with_bellman_ford : (string, int) graph -> int -> string -> unit =
  let pc g s = bellman_ford g s |> fst |> fst in
  graphviz_with_min_paths pc

let graphviz_with_dijkstra : (string, int) graph -> int -> string -> unit =
  let pc g s = dijkstra g s |> fst in
  graphviz_with_min_paths pc

(***********************************)
(*   Working with Shortest Paths   *)
(***********************************)

let get_shortest_path (p : (int * int) NodeTable.hash_table) (s : int) (u : int)
    : (int * int) list option
  =
  let rec walk acc v =
    match NodeTable.get p v with
    | None -> acc
    | Some x -> walk ((x, v) :: acc) x
  in
  let res = walk [] u in
  if u = s || (res <> [] && List.hd res |> fst = s) then Some res else None

let rec get_path_weigth (g : ('a, int) graph) (path : (int * int) list) : int =
  match path with
  | (u, v) :: t ->
    let w = get_linked_edge_label g u v in
    w + get_path_weigth g t
  | _ -> 0

(***************************************)
(*    Testing for SS shortest paths    *)
(***************************************)

(*

What is provided for testing:

* p - predecessor tree
* d - distance table
* g - the graph
* s - source node
* u - destination node

*)

(* 1. Path is connected *)
let test_path_connected
    (p : (int * int) NodeTable.hash_table)
    (_d : (int * Distance.dist) NodeTable.hash_table)
    (_g : ('a, int) graph)
    (s : int)
    (u : int)
    : bool
  =
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let rec walk p =
      match p with
      | (_u, v) :: (x, y) :: t -> v = x && walk ((x, y) :: t)
      | _ -> true
    in
    walk path

(* 2. Path's weight is correctly recorded *)
let test_path_weight
    (p : (int * int) NodeTable.hash_table)
    (d : (int * Distance.dist) NodeTable.hash_table)
    (g : ('a, int) graph)
    (s : int)
    (u : int)
    : bool
  =
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let w1 = get_path_weigth g path in
    let w2 = get_exn @@ NodeTable.get d u |> Distance.int_of_dist in
    w1 = w2

(* 3. Has all edges *)
let test_that_is_path_graph
    (p : (int * int) NodeTable.hash_table)
    (_d : (int * Distance.dist) NodeTable.hash_table)
    (g : ('a, int) graph)
    (s : int)
    (u : int)
    : bool
  =
  match get_shortest_path p s u with
  | None -> true
  | Some path ->
    let all_edges = g.edges |> elements in
    List.for_all (fun e -> List.mem e all_edges) path

(* 4. Exists for any reachable node *)
let test_reachable_hence_has_path
    (p : (int * int) NodeTable.hash_table)
    (_d : (int * Distance.dist) NodeTable.hash_table)
    (g : ('a, int) graph)
    (s : int)
    (u : int)
    : bool
  =
  if is_reachable g s u then get_shortest_path p s u <> None else true

(* 5. And is the shortest *)
let test_shortest_is_shorter
    (p : (int * int) NodeTable.hash_table)
    (_d : (int * Distance.dist) NodeTable.hash_table)
    (g : ('a, int) graph)
    (s : int)
    (u : int)
    : bool
  =
  match reachable g s u with
  | None -> true
  | Some p1 ->
    (match get_shortest_path p s u with
    | None -> false
    | Some p2 ->
      let w1 = get_path_weigth g p1 in
      let w2 = get_path_weigth g p2 in
      w2 <= w1)

(*  Main testing function  *)

let test_sssp
    (algo :
      ('a, int) graph ->
      int ->
      (int * int) NodeTable.hash_table * (int * Distance.dist) NodeTable.hash_table)
    (g : ('a, int) graph)
    : bool
  =
  let all_nodes = get_nodes g in
  List.iter
    (fun u ->
      List.iter
        (fun v ->
          let p, d = algo g u in
          assert (test_path_connected p d g u v);
          assert (test_path_weight p d g u v);
          assert (test_that_is_path_graph p d g u v);
          assert (test_reachable_hence_has_path p d g u v);
          assert (test_shortest_is_shorter p d g u v))
        all_nodes)
    all_nodes;
  true

(***********************************)
(*           Examples              *)
(***********************************)

(*     Bellman-Ford Example   *)

let bf_example_nodes : string array = [|"s"; "t"; "y"; "x"; "z"|]

let bf_example_edges : (int * int) list =
  [(0, 1); (0, 2); (1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 1); (4, 0); (4, 3)]

let bf_example_labels : (int * int * int) list =
  [
    (0, 1, 6);
    (0, 2, 7);
    (1, 2, 8);
    (1, 3, 5);
    (1, 4, -4);
    (2, 3, -3);
    (2, 4, 4);
    (3, 1, -2);
    (4, 0, 2);
    (4, 3, 7);
  ]

(* Bellman-Ford example graph *)
let example_graph_bf : (string, int) graph =
  read_graph_and_payloads 5 bf_example_nodes bf_example_edges bf_example_labels

(*     Dijkstra Example   *)

let dijkstra_example_nodes : string array = [|"s"; "t"; "y"; "x"; "z"|]

let dijkstra_example_edges : (int * int) list =
  [(0, 1); (0, 2); (1, 2); (1, 3); (2, 1); (2, 3); (2, 4); (3, 4); (4, 0); (4, 3)]

let dijkstra_example_labels : (int * int * int) list =
  [
    (0, 1, 10);
    (0, 2, 5);
    (1, 2, 2);
    (1, 3, 1);
    (2, 1, 3);
    (2, 3, 9);
    (2, 4, 2);
    (3, 4, 4);
    (4, 0, 7);
    (4, 3, 6);
  ]

let example_graph_dijkstra : (string, int) graph =
  read_graph_and_payloads
    5
    dijkstra_example_nodes
    dijkstra_example_edges
    dijkstra_example_labels
