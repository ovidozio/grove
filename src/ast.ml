(* ast.ml *)

type id = ID of string
type label = Label of string

type position_entry =
  | Coord of int * int
  | Ref of id

type node_shape =
  | TextOnly
  | Circle

type arrow_style =
  | Solid
  | Dashed

type arrow_path_kind =
  | Straight
  | Curved

type canvas = {
  width : int;
  height : int;
}

type node = {
  id : id option;
  label : label;
  pos : int * int;
  shape : node_shape;
}

type arrow = {
  id : id option;
  label : label;
  start_pos : position_entry;
  end_pos : position_entry;
  style : arrow_style;
  path_kind : arrow_path_kind;
}

type diagram = {
  canvas : canvas;
  nodes : node list;
  arrows : arrow list;
}

