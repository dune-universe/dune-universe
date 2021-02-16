type statement =
  | Graph of (string * string) list
  | Node of int * (string * string) list
  | Edge of int * int * (string * string) list

type file = statement option list

type point = float * float

type path = point list

type node = int * point

type edge = int * int * path

type digraph = {
  bounding_box : point * point;
  nodes : node list;
  edges : edge list;
}
