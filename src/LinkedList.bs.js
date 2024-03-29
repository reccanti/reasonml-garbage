// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

function init(param) {
  return /* Empty */0;
}

function append(list, value) {
  if (list) {
    return /* Node */[
            list[0],
            append(list[1], value)
          ];
  } else {
    return /* Node */[
            value,
            /* Empty */0
          ];
  }
}

var printVal = (
  function printVal(value) {
    console.log("Value:", value);
  }
);

function printList(_list) {
  while(true) {
    var list = _list;
    if (list) {
      Curry._1(printVal, list[0]);
      _list = list[1];
      continue ;
    } else {
      return /* () */0;
    }
  };
}

var list = append(/* Node */[
      4,
      /* Empty */0
    ], 6);

printList(list);

exports.init = init;
exports.append = append;
exports.printVal = printVal;
exports.printList = printList;
exports.list = list;
/* printVal Not a pure module */
