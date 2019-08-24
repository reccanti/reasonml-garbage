type linkedList('a) =
  | Empty
  | Node('a, linkedList('a));

/**
 * Initialize an empty linked list
 */
let init = () => Empty;

/**
 * Append a node to the end of the list
 */
let rec insert = (list: linkedList('a), value: 'a) => {
  switch (list) {
  // if it's empty, we've hit the end of the list,
  // so insert the new Node at the end
  | Empty => Node(value, Empty)
  | Node(val_, next) =>
    val_ >= value
      // If the current value is greater than the value
      // we're inserting, we should put it before that
      // value
      ? Node(value, list)
      // Otherwise, continue searching for the next
      // location in the list to insert the value
      : Node(val_, insert(next, value))
  };
};

/**
 * Print all elements in a list
 */
// a convenience function for printing values
let printVal = [%raw
  {|
  function printVal(value) {
    console.log("Value:", value);
  }
|}
];

let rec printList = (list: linkedList('a)) => {
  switch (list) {
  | Node(value, next) =>
    printVal(value);
    printList(next);
  | Empty => ()
  };
};

// let list = insert(insert(insert(insert(init(), 5), 7), 6), 1);
let list = init()->insert(1)->insert(5)->insert(3);
printList(list);