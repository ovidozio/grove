(* svg.ml *)

let svg_header width height =
  Printf.sprintf
    "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d'>\n"
    width height width height

let svg_footer = "</svg>\n"

let path_string (kind : Layout.path_kind) (x1, y1) (x2, y2) =
  match kind with
  | Layout.Straight ->
      Printf.sprintf "M %d %d L %d %d" x1 y1 x2 y2
  | Layout.Curved ->
      let cx = (x1 + x2) / 2 in
      let cy = (y1 + y2) / 2 - 40 in
      Printf.sprintf "M %d %d Q %d %d %d %d" x1 y1 cx cy x2 y2

let stroke_dasharray (style : Layout.arrow_style) =
  match style with
  | Layout.Solid -> ""
  | Layout.Dashed -> "5,5"

let draw_marker_def () =
  "<defs>
    <marker id='arrowhead' markerWidth='10' markerHeight='7'
            refX='10' refY='3.5' orient='auto'>
      <polygon points='0 0, 10 3.5, 0 7' fill='black'/>
    </marker>
  </defs>\n"

let draw_arrow (arrow : Layout.resolved_arrow) =
  let (x1, y1) = arrow.start_pos in
  let (x2, y2) = arrow.end_pos in
  let d = path_string arrow.path_kind (x1, y1) (x2, y2) in

  let background =
    Printf.sprintf
      "<path d='%s' stroke='white' stroke-width='10' fill='none' stroke-linecap='round'/>\n"
      d
  in

  let foreground =
    Printf.sprintf
      "<path d='%s' stroke='black' stroke-width='2' fill='none' marker-end='url(#arrowhead)' stroke-linecap='round'%s />\n"
      d
      (let dash = stroke_dasharray arrow.style in
       if dash = "" then "" else Printf.sprintf " stroke-dasharray='%s'" dash)
  in

  background ^ foreground

let extract_label (lbl : Layout.label) =
  match lbl with
  | Layout.Label s -> s

let draw_node (node : Layout.resolved_node) =
  let (x, y) = node.pos in
  let label_text = extract_label node.label in
  Printf.sprintf
    "  <circle cx='%d' cy='%d' r='10' fill='lightblue' stroke='black' stroke-width='1' />\n\
     <text x='%d' y='%d' font-size='10' text-anchor='middle' dy='.3em'>%s</text>\n"
    x y x (y + 20) label_text

let render (diagram : Layout.resolved_diagram) : string =
  let { Layout.canvas; nodes; arrows } = diagram in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (svg_header canvas.width canvas.height);
  Buffer.add_string buf (draw_marker_def ());
  List.iter (fun node -> Buffer.add_string buf (draw_node node)) nodes;
  List.iter (fun arrow -> Buffer.add_string buf (draw_arrow arrow)) arrows;
  Buffer.add_string buf svg_footer;
  Buffer.contents buf

