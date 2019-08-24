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
let rec append = (list: linkedList('a), value: 'a) => {
  switch (list) {
  | Empty => Node(value, Empty)
  | Node(val_, next) => Node(val_, append(next, value))
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

let list = append(append(init(), 4), 6);

printList(list);