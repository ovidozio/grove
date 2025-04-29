(* svg.ml *)

open Layout

let svg_header width height =
  Printf.sprintf
    "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d'>\n"
    width height width height

let svg_footer = "</svg>\n"

let draw_marker_def () =
  "<defs>
    <marker id='arrowhead' markerWidth='10' markerHeight='7'
            refX='10' refY='3.5' orient='auto'>
      <polygon points='0 0, 10 3.5, 0 7' fill='black'/>
    </marker>
  </defs>\n"

let draw_arrow (arrow : resolved_arrow) =
  let (x1, y1) = arrow.start_pos in
  let (x2, y2) = arrow.end_pos in

  (* background path - fat white line *)
  let background =
    Printf.sprintf
      "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='white' stroke-width='10' stroke-linecap='round' />\n"
      x1 y1 x2 y2
  in

  (* foreground path - thin black line with arrowhead *)
  let foreground =
    Printf.sprintf
      "<line x1='%d' y1='%d' x2='%d' y2='%d' stroke='black' stroke-width='2' marker-end='url(#arrowhead)' stroke-linecap='round' />\n"
      x1 y1 x2 y2
  in

  background ^ foreground

let render (diagram : resolved_diagram) : string =
  let { canvas; arrows; _ } = diagram in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (svg_header canvas.width canvas.height);
  Buffer.add_string buf (draw_marker_def ());
  List.iter (fun (arrow : resolved_arrow) -> Buffer.add_string buf (draw_arrow arrow)) arrows;
  Buffer.add_string buf svg_footer;
  Buffer.contents buf

