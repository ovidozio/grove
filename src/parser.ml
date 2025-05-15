(* parser.ml *)

exception ParseError of string

type parser_state = {
  tokens : Lexer.token list;
  current : Lexer.token;
}

(* Initialize parser state *)
let init_parser tokens =
  match tokens with
  | [] -> raise (ParseError "Empty token stream")
  | t :: rest -> { tokens = rest; current = t }

(* Advance to next token *)
let advance state =
  match state.tokens with
  | [] -> { tokens = []; current = Lexer.EOF }
  | hd :: tl -> { tokens = tl; current = hd }

(* Check if an ID is reserved *)
let is_reserved_id = function
  | "canvas" | "node" | "arrow" | "dashed" | "curved" | "circle" -> true
  | _ -> false

(* Parse an ID or group reference *)
let parse_reference_id state =
  match state.current with
  | Lexer.ID name when not (is_reserved_id name) ->
      let state = advance state in
      (Ast.ID name, state)
  | Lexer.GROUP text ->
      let state = advance state in
      (Ast.ID text, state)
  | _ -> raise (ParseError "Expected a reference ID or group")

(* Parse a defined ID (must be a LABEL) *)
let parse_defined_id state =
  match state.current with
  | Lexer.LABEL text ->
      let state = advance state in
      (Ast.ID text, state)
  | _ -> raise (ParseError "Expected a label (quoted string) to define an ID")

(* Parse a label *)
let parse_label state =
  match state.current with
  | Lexer.LABEL text ->
      let state = advance state in
      (Ast.Label text, state)
  | _ -> raise (ParseError "Expected a label (quoted string)")

(* Parse an optional label *)
let parse_optional_label state =
  match state.current with
  | Lexer.LABEL _ ->
      let (label, state) = parse_label state in
      (Some label, state)
  | _ -> (None, state)

(* Parse a number *)
let expect_number state =
  match state.current with
  | Lexer.NUMBER n ->
      let state = advance state in
      (n, state)
  | _ -> raise (ParseError "Expected a number")

(* Expect a specific token *)
let expect_token expected state =
  if state.current = expected then
    advance state
  else
    raise (ParseError "Expected a different token")

(* Parse a position (Coord or Ref) *)
let parse_position state =
  match state.current with
  | Lexer.LPAREN ->
      let state = advance state in
      let (x, state) = expect_number state in
      let state = expect_token Lexer.COMMA state in
      let (y, state) = expect_number state in
      let state = expect_token Lexer.RPAREN state in
      (Ast.Coord (x, y), state)
  | Lexer.ID _ | Lexer.GROUP _ ->
      let (id, state) = parse_reference_id state in
      (Ast.Ref id, state)
  | _ -> raise (ParseError "Expected a position (coord or id reference)")

(* Parse node shape *)
let parse_node_shape state =
  match state.current with
  | Lexer.ID "circle" -> (Ast.Circle, advance state)
  | _ -> (Ast.TextOnly, state)

(* Parse arrow modifiers *)
let parse_arrow_modifiers state =
  let rec consume style path_kind state =
    match state.current with
    | Lexer.ID "dashed" -> consume Ast.Dashed path_kind (advance state)
    | Lexer.ID "curved" -> consume style Ast.Curved (advance state)
    | _ -> (style, path_kind, state)
  in
  consume Ast.Solid Ast.Straight state

(* Parse canvas *)
let parse_canvas state =
  match state.current with
  | Lexer.ID "canvas" ->
      let state = advance state in
      let (width, state) = expect_number state in
      let (height, state) = expect_number state in
      let state = expect_token Lexer.SEMICOLON state in
      ({ Ast.width = width; Ast.height = height }, state)
  | _ -> raise (ParseError "Expected 'canvas' at the beginning")

(* Parse a node *)
let parse_node state =
  let (shape, state) = parse_node_shape state in
  let (id, state) = parse_defined_id state in
  let (label, state) = parse_label state in
  let (pos_entry, state) = parse_position state in
  let pos =
    match pos_entry with
    | Ast.Coord (x, y) -> (x, y)
    | Ast.Ref _ -> raise (ParseError "Node position must be explicit coordinates")
  in
  let state = expect_token Lexer.SEMICOLON state in
  ({ Ast.id = Some id; Ast.label = label; Ast.pos = pos; Ast.shape = shape }, state)

(* Parse an arrow *)
let parse_arrow state =
  let (style, path_kind, state) = parse_arrow_modifiers state in
  let (id_opt, state) =
    match state.current with
    | Lexer.LABEL _ ->
        let (id, state) = parse_defined_id state in
        (Some id, state)
    | _ -> (None, state)
  in
  let (label_opt, state) = parse_optional_label state in
  let (start_pos, state) = parse_position state in
  let (end_pos, state) = parse_position state in
  let state = expect_token Lexer.SEMICOLON state in
  ({
    Ast.id = id_opt;
    Ast.label = (match label_opt with Some lbl -> lbl | None -> Ast.Label "");
    Ast.start_pos = start_pos;
    Ast.end_pos = end_pos;
    Ast.style = style;
    Ast.path_kind = path_kind;
  }, state)

(* Parse list of nodes *)
let rec parse_nodes state =
  match state.current with
  | Lexer.ID "node" ->
      let state = advance state in
      let (n, state) = parse_node state in
      let (ns, state) = parse_nodes state in
      (n :: ns, state)
  | _ -> ([], state)

(* Parse list of arrows *)
let rec parse_arrows state =
  match state.current with
  | Lexer.ID "arrow" ->
      let state = advance state in
      let (a, state) = parse_arrow state in
      let (as_, state) = parse_arrows state in
      (a :: as_, state)
  | _ -> ([], state)

(* Parse full diagram *)
let parse_diagram tokens =
  let state = init_parser tokens in
  let (canvas, state) = parse_canvas state in
  let (nodes, state) = parse_nodes state in
  let (arrows, _) = parse_arrows state in
  { Ast.canvas = canvas; Ast.nodes = nodes; Ast.arrows = arrows }

