(* layout.ml *)

exception LayoutError of string

type label = Label of string

type resolved_arrow = {
  id : Ast.id option;
  label : label;
  start_pos : int * int;
  end_pos : int * int;
}

type resolved_node = {
  id : Ast.id option;
  label : label;
  pos : int * int;
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
  }

let resolve_positions (diagram : Ast.diagram) : resolved_diagram =
  (* Build id -> position map from nodes *)
  let id_map =
    List.fold_left (fun acc (node : Ast.node) ->
      match node.Ast.id with
      | Some (Ast.ID id_str) -> (id_str, node.Ast.pos) :: acc
      | None -> acc
    ) [] diagram.Ast.nodes
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

  let resolved_arrows =
    List.map (fun (arrow : Ast.arrow) ->
      {
        id = arrow.Ast.id;
        label = resolve_label arrow.Ast.label;
        start_pos = resolve_pos arrow.Ast.start_pos;
        end_pos = resolve_pos arrow.Ast.end_pos;
      }
    ) diagram.Ast.arrows
  in

  let resolved_nodes =
    List.map resolve_node diagram.Ast.nodes
  in

  {
    canvas = {
      width = diagram.Ast.canvas.Ast.width;
      height = diagram.Ast.canvas.Ast.height;
    };
    nodes = resolved_nodes;
    arrows = resolved_arrows;
  }

