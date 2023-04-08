open ArrayUtil

module UnionFind = struct
  type t = {
    count : int ref;
    id : int array;
  }

  let mk_UF n =
    let ints = list_to_array (iota (n - 1)) in
    { count = ref n; id = ints }

  let get_count uf = !(uf.count)

  (* Question: What is the complexity of find? *)
  let find uf p =
    let r = ref p in
    while !r <> uf.id.(!r) do
      r := uf.id.(!r)
    done;
    !r

  (* Question: What is the complexity of union? *)
  let union uf p q =
    let i = find uf p in
    let j = find uf q in
    if i = j
    then ()
    else (
      uf.id.(i) <- j;
      uf.count := !(uf.count) - 1)

  let connected uf p q = find uf p = find uf q

  let print_uf uf =
    let n = Array.length uf.id in
    let ids = iota (n - 1) in
    for i = 0 to n - 1 do
      let connected = List.find_all (fun e -> find uf e = i) ids in
      if connected <> []
      then (
        Printf.printf "Class %d: [" i;
        List.iter (fun j -> Printf.printf "%d; " j) connected;
        print_endline "]")
    done
end
