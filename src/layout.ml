(* layout.ml *)

exception LayoutError of string

type label = Label of string

type path_kind =
  | Straight
  | Curved

type arrow_style =
  | Solid
  | Dashed

type resolved_node = {
  id : Ast.id option;
  label : label;
  pos : int * int;
  shape : Ast.node_shape;
}

type resolved_arrow = {
  id : Ast.id option;
  label : label;
  start_pos : int * int;
  end_pos : int * int;
  style : arrow_style;
  path_kind : path_kind;
}

type resolved_canvas = {
  width : int;
  height : int;
}

type resolved_diagram = {
  canvas : resolved_canvas;
  nodes : resolved_node list;
  arrows : resolved_arrow list;
}

let resolve_label (l : Ast.label) : label =
  match l with
  | Ast.Label s -> Label s

let resolve_node (n : Ast.node) : resolved_node =
  {
    id = n.Ast.id;
    label = resolve_label n.Ast.label;
    pos = n.Ast.pos;
    shape = n.Ast.shape;
  }

let resolve_positions (diagram : Ast.diagram) : resolved_diagram =
  (* Build a map from id -> position for resolving references *)
  let id_map =
    List.fold_left (fun acc (node : Ast.node) ->
      match node.id with
      | Some (Ast.ID id_str) -> (id_str, node.pos) :: acc
      | None -> acc
    ) [] diagram.nodes
  in

  let find (id : Ast.id) =
    match id with
    | Ast.ID id_str ->
        (match List.assoc_opt id_str id_map with
         | Some pos -> pos
         | None -> raise (LayoutError ("Unknown reference: " ^ id_str)))
  in

  let resolve_pos (p : Ast.position_entry) =
    match p with
    | Ast.Coord (x, y) -> (x, y)
    | Ast.Ref id -> find id
  in

  let convert_style (s : Ast.arrow_style) : arrow_style =
    match s with
    | Ast.Solid -> Solid
    | Ast.Dashed -> Dashed
  in

  let convert_path_kind (p : Ast.arrow_path_kind) : path_kind =
    match p with
    | Ast.Straight -> Straight
    | Ast.Curved -> Curved
  in

  let resolved_arrows =
    List.map (fun (arrow : Ast.arrow) ->
      {
        id = arrow.id;
        label = resolve_label arrow.label;
        start_pos = resolve_pos arrow.start_pos;
        end_pos = resolve_pos arrow.end_pos;
        style = convert_style arrow.style;
        path_kind = convert_path_kind arrow.path_kind;
      }
    ) diagram.arrows
  in

  let resolved_nodes =
    List.map resolve_node diagram.nodes
  in

  {
    canvas = {
      width = diagram.canvas.width;
      height = diagram.canvas.height;
    };
    nodes = resolved_nodes;
    arrows = resolved_arrows;
  }

