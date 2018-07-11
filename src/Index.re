module LinkedList = {
  type linked_list('a) =
    | Empty
    | Node('a, linked_list('a));

  let rec make_tail = (length: int, value: 'a) =>
    switch (length) {
    | 1 => Node(value, Empty)
    | len when len > 1 => Node(value, make_tail(length - 1, value))
    | _ => raise(Invalid_argument("Length below 1"))
    };

  let make = (length: int, value: 'a) =>
    switch (length) {
    | 0 => Empty
    | len when len > 0 => make_tail(len, value)
    | _ => raise(Invalid_argument("Length below 0"))
    };

  let rec iter = (fn: 'a => unit, linked_list) =>
    switch (linked_list) {
    | Empty => (() => ())
    | Node(head, rest) =>
      fn(head);
      iter(fn, rest);
    };

  let data = linked_list =>
    switch (linked_list) {
    | Empty => raise(Not_found)
    | Node(data, _) => data
    };

  let next = linked_list =>
    switch (linked_list) {
    | Empty => raise(Not_found)
    | Node(_data, Empty) => raise(Not_found)
    | Node(_data, next_node) => next_node
    };
};

/* let linked_list: LinkedList.linked_list(int) =
   LinkedList.Node(25, LinkedList.Node(27, LinkedList.Empty)); */

/* make linked list with 3 nodes */
let linked_list: LinkedList.linked_list(int) = LinkedList.make(3, 22);

/* print the data from the first node */
print_endline(string_of_int(LinkedList.data(linked_list)));

/* get the next node, print it's data */
let next_node = LinkedList.next(linked_list);
print_endline(string_of_int(LinkedList.data(next_node)));

let next_node_2 = LinkedList.next(next_node);
print_endline(string_of_int(LinkedList.data(next_node_2)));

/* let next_node_3 = LinkedList.next(next_node_2); */
/* print_endline(string_of_int(LinkedList.data(next_node_3))); */

print_endline("Iterate over the linked list");
LinkedList.iter(item => print_endline(string_of_int(item)), linked_list);