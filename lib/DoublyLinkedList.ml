open Util

(***********************************************)
(*             Doubly Linked Lists             *)
(***********************************************)

module DoublyLinkedList = struct
  type 'e dll_node = {
    value : 'e ref;
    prev : 'e dll_node option ref;
    next : 'e dll_node option ref;
  }

  type 'e t = 'e dll_node option

  let mk_node (e : 'a) : 'a dll_node = { value = ref e; prev = ref None; next = ref None }

  let prev (n : 'a dll_node) : 'a dll_node option = !(n.prev)

  let next (n : 'a dll_node) : 'a dll_node option = !(n.next)

  let value (n : 'a dll_node) : 'a = !(n.value)

  let set_value (n : 'a dll_node) (v : 'a) : unit = n.value := v

  let insert_after (n1 : 'a dll_node) (n2 : 'a dll_node) : unit =
    let n3 = next n1 in
    (match n3 with
    | Some n -> n.prev := Some n2
    | _ -> ());
    n2.next := n3;
    n1.next := Some n2;
    n2.prev := Some n1

  let insert_before (n1 : 'a dll_node) (n2 : 'a dll_node) : unit =
    let n0 = prev n2 in
    (match n0 with
    | Some n -> n.next := Some n1
    | _ -> ());
    n1.prev := n0;
    n1.next := Some n2;
    n2.prev := Some n1

  let to_list_from (n : 'a dll_node) : 'a list =
    let res = ref [] in
    let iter = ref (Some n) in
    while !iter <> None do
      let node = get_exn !iter in
      res := value node :: !res;
      iter := next node
    done;
    List.rev !res

  let remove (n : 'a dll_node) : unit =
    (match prev n with
    | None -> ()
    | Some p -> p.next := next n);
    match next n with
    | None -> ()
    | Some nxt -> nxt.prev := prev n
end
