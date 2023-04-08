open Util
open ReadingFiles
open Graphs
open LinkedGraphs

(*****************************************)
(*     Reading a graph with payloads     *)
(*****************************************)

let read_graph_and_payloads
    (size : int)
    (nvalue : 'a array)
    (elist : (int * int) list)
    (elabels : (int * int * 'b) list)
    : ('a, 'b) graph
  =
  let open AdjacencyGraphs in
  let g = mk_graph size in
  for i = 0 to g.size - 1 do
    set_payload g i nvalue.(i)
  done;
  List.iter (fun (s, d) -> add_edge g s d) elist;
  List.iter (fun (s, d, l) -> set_edge_label g s d l) elabels;
  LinkedGraphs.from_simple_adjacency_graph g

(*****************************************)
(*        1. Reachability queries        *)
(*****************************************)

let reachable (g : ('a, 'b) graph) (init : int) (final : int) : (int * int) list option =
  let rec walk path visited n =
    if n = final
    then Some path
    else if List.mem n visited
    then None
    else (
      (* Try successors *)
      let node = get_node g n in
      let successors = get_next node in
      let visited' = n :: visited in
      let rec iter = function
        | [] -> None
        | h :: t ->
          let path' = (n, h) :: path in
          (match walk path' visited' h with
          | Some p -> Some p
          | None -> iter t)
      in
      iter successors)
  in
  match walk [] [] init with
  | Some p -> Some (List.rev p)
  | _ -> None

let is_reachable (g : ('a, 'b) graph) (init : int) (final : int) : bool =
  reachable g init final <> None

(* Graphical representation *)

(*

Taking attributes from_simple_adjacency_graph
https://graphs.grevian.org/example
*)

let bold_edge : string = "[color=red,penwidth=3.0]"

let graphviz_with_path (g : ('a, 'b) graph) (init : int) (final : int) (out : string)
    : unit
  =
  let r = reachable g init final in
  let attrib (s, d) =
    match r with
    | None -> ""
    | Some p -> if List.mem (s, d) p then bold_edge else ""
  in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s = graphviz_string_of_graph "digraph" " -> " string_of_int attrib ag in
  write_string_to_file out s

(*****************************************)
(*          Depth-first search           *)
(*****************************************)

module GraphDFS = struct
  open NodeTable

  type color =
    | White
    | Gray
    | Black

  let dfs (g : ('a, 'b) graph)
      : int list * (int * int list) hash_table * (int * (int * int)) hash_table * bool
    =
    let color_map = mk_new_table (v_size g) in
    let tree_map = mk_new_table (v_size g) in
    let time_map = mk_new_table (v_size g) in
    let has_cycles = ref false in
    let roots = ref [] in
    let all_nodes = get_nodes g in
    (* Make all nodes white *)
    List.iter (fun n -> insert color_map n White) all_nodes;
    (* Insert all nodes to the tree *)
    List.iter (fun n -> insert tree_map n []) all_nodes;
    let time = ref 0 in
    let rec dfs_visit u =
      time := !time + 1;
      let u_in = !time in
      insert color_map u Gray;
      get_succ g u
      |> List.iter (fun v ->
             let v_color = get_exn @@ get color_map v in
             if v_color = White
             then (
               let siblings = get_exn @@ get tree_map u in
               insert tree_map u (v :: siblings);
               dfs_visit v)
             else if v_color = Gray
             then has_cycles := true);
      insert color_map u Black;
      time := !time + 1;
      let u_out = !time in
      insert time_map u (u_in, u_out)
    in
    List.iter
      (fun n ->
        if get_exn @@ get color_map n = White
        then (
          (* Record roots *)
          roots := n :: !roots;
          dfs_visit n))
      all_nodes;
    (!roots, tree_map, time_map, !has_cycles)

  (* Visualise with DFS *)
  let graphviz_with_dfs (g : ('a, 'b) graph) (out : string) : unit =
    let _, tree, _, _ = dfs g in
    let eattrib (s, d) =
      match get tree s with
      | None -> ""
      | Some p -> if List.mem d p then bold_edge else ""
    in
    let ag = LinkedGraphs.to_adjacency_graph g in
    let s = graphviz_string_of_graph "digraph" " -> " string_of_int eattrib ag in
    write_string_to_file out s

  (* DFS-induced search *)
  let is_reachable_via_dfs (g : ('a, 'b) graph) (init : int) (final : int) : bool =
    let roots, tree, _, _ = dfs g in
    let rec walk n =
      if n = final then true else get tree n |> get_exn |> List.exists (fun v -> walk v)
    in
    if List.mem init roots then walk init else false

  (* Question: is reachability equivalent to DFS-reachability *)
end

(***********************************)
(* Testing Reachability via DFS    *)
(***********************************)

let test_dfs (g : ('a, 'b) graph) : bool =
  let all_nodes = LinkedGraphs.get_nodes g in
  let dfs_roots, _, _, _ = GraphDFS.dfs g in
  (* Any node DFS-reachable from a root r is reachable from r *)
  (* ?? Is it true or false? *)
  let fact1 =
    List.for_all
      (fun u ->
        List.for_all
          (fun v ->
            if GraphDFS.is_reachable_via_dfs g u v then is_reachable g u v else true)
          all_nodes)
      dfs_roots
  in
  (* Any node is reachable from some root r *)
  (* ?? Is it true or false? *)
  let fact2 =
    List.for_all
      (fun u -> List.exists (fun r -> GraphDFS.is_reachable_via_dfs g r u) dfs_roots)
      all_nodes
  in
  fact1 && fact2

(*****************************************)
(*            Topological sort           *)
(*****************************************)

let graphviz_with_payload (g : ('a, 'b) graph) (values : string array) (out : string)
    : unit
  =
  let eattrib _e = "" in
  let vattrib n = values.(n) in
  let ag = LinkedGraphs.to_adjacency_graph g in
  let s = graphviz_string_of_graph "digraph" " -> " vattrib eattrib ag in
  write_string_to_file out s

module TopologicalSort = struct
  open NodeTable

  let get_last_time (m : (int * 'a) hash_table) (n : int) : 'a = get_exn @@ get m n

  let topo_sort (g : ('a, 'b) graph) : int list =
    let _, _, time_map, _ = GraphDFS.dfs g in
    get_nodes g
    |> List.sort (fun n1 n2 ->
           let _, t1 = get_last_time time_map n1 in
           let _, t2 = get_last_time time_map n2 in
           if t1 < t2 then 1 else if t1 > t2 then -1 else 0)
end

let clothes : string array =
  [|
    "underpants"; "phone"; "shoes"; "shirt"; "tie"; "jacket"; "socks"; "belt"; "trousers";
  |]

let clothes_edges : (int * int) list =
  [(0, 8); (0, 2); (8, 2); (8, 1); (8, 7); (3, 7); (3, 4); (4, 5); (7, 5); (6, 2)]

let clothes_graph : (string, unit) graph =
  read_graph_and_payloads 9 clothes clothes_edges ([] : (int * int * unit) list)

let rec all_pairs (ls : 'a list) : ('a * 'a) list =
  match ls with
  | [] -> []
  | [_] -> []
  | h1 :: h2 :: t -> (h1, h2) :: all_pairs (h2 :: t)
