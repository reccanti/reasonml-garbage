type node('a) = {
  value: 'a,
  next: option(node('a)),
};

/**
 * Initializes a single Node with no next value
 */
let init = (value: 'a) => {value, next: None};

/**
 * Inserts a new node at the end of the linked list
 */
let rec append = (node: node('a), value: 'a) => {
  switch (node.next) {
  | Some(nextNode) => {...node, next: Some(append(nextNode, value))}
  | None =>
    let newNode = {value, next: None};
    {...node, next: Some(newNode)};
  };
};

let x = init(4);
let y = append(x, 6);
let z = append(y, 9);
let w = append(z, 24);

Js.log(w.next);