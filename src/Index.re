module LinkedList = {
  type linked_list('a) =
    | Empty
    | Node('a, linked_list('a));

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

/* create linked list with 2 nodes */
let linked_list: LinkedList.linked_list(int) =
  LinkedList.Node(25, LinkedList.Node(27, LinkedList.Empty));

/* print the data from the first node */
print_endline(string_of_int(LinkedList.data(linked_list)));

/* get the next node, print it's data */
let next_node = LinkedList.next(linked_list);
print_endline(string_of_int(LinkedList.data(next_node)));