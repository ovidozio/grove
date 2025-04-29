(* ast.ml *)
type id = ID of string
type label = Label of string

type position_entry =
  | Coord of int * int
  | Ref of id

type canvas = {
  width : int;
  height : int;
}

type node = {
  id : id option;
  label : label;
  pos : int * int;
}

type arrow = {
  id : id option;
  label : label;
  start_pos : position_entry;
  end_pos : position_entry;
}

type diagram = {
  canvas : canvas;
  nodes : node list;
  arrows : arrow list;
}

