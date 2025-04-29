(* main.ml *)

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc

let () =
  if Array.length Sys.argv <> 3 then (
    Printf.eprintf "Usage: %s input.txt output.svg\n" Sys.argv.(0);
    exit 1
  );

  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in

  let input_text = read_file input_file in
  let tokens = Grove.Lexer.tokenize input_text in
  let ast_diagram = Grove.Parser.parse_diagram tokens in
  let resolved_diagram = Grove.Layout.resolve_positions ast_diagram in
  let svg_output = Grove.Svg.render resolved_diagram in

  write_file output_file svg_output;
  Printf.printf "Generated SVG: %s\n" output_file

