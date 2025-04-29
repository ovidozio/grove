(* parser.ml *)

exception ParseError of string

type parser_state = {
  tokens : Lexer.token list;
  current : Lexer.token;
}

let init_parser tokens =
  match tokens with
  | [] -> raise (ParseError "Empty token stream")
  | t :: rest -> { tokens = rest; current = t }

let advance state =
  match state.tokens with
  | [] -> { tokens = []; current = Lexer.EOF }
  | hd :: tl -> { tokens = tl; current = hd }

let expect_id state =
  match state.current with
  | Lexer.ID name ->
      let state = advance state in
      (Ast.ID name, state)
  | _ -> raise (ParseError "Expected an identifier")

let expect_label state =
  match state.current with
  | Lexer.LABEL text ->
      let state = advance state in
      (Ast.Label text, state)
  | _ -> raise (ParseError "Expected a label (quoted string)")

let expect_number state =
  match state.current with
  | Lexer.NUMBER n ->
      let state = advance state in
      (n, state)
  | _ -> raise (ParseError "Expected a number")

let expect_token expected state =
  if state.current = expected then
    advance state
  else
    raise (ParseError "Expected a different token")

let parse_canvas state =
  match state.current with
  | Lexer.ID "canvas" ->
      let state = advance state in
      let (width, state) = expect_number state in
      let (height, state) = expect_number state in
      let state = expect_token Lexer.SEMICOLON state in
      ({ Ast.width = width; Ast.height = height }, state)
  | _ -> raise (ParseError "Expected 'canvas' at the beginning")

let parse_position state =
  match state.current with
  | Lexer.LPAREN ->
      let state = advance state in
      let (x, state) = expect_number state in
      let state = expect_token Lexer.COMMA state in
      let (y, state) = expect_number state in
      let state = expect_token Lexer.RPAREN state in
      (Ast.Coord (x, y), state)
  | Lexer.ID _ ->
      let (id, state) = expect_id state in
      (Ast.Ref id, state)
  | _ -> raise (ParseError "Expected a position (coord or id reference)")

let parse_optional_id state =
  match state.current with
  | Lexer.ID _ ->
      let (id, state) = expect_id state in
      (Some id, state)
  | _ -> (None, state)

let parse_node state =
  let (id_opt, state) = parse_optional_id state in
  let (lbl, state) = expect_label state in
  let (pos_entry, state) = parse_position state in
  let pos =
    match pos_entry with
    | Ast.Coord (x, y) -> (x, y)
    | Ast.Ref _ -> raise (ParseError "Node position must be explicit coordinates, not a reference")
  in
  let state = expect_token Lexer.SEMICOLON state in
  ({ Ast.id = id_opt; Ast.label = lbl; Ast.pos = pos }, state)

let parse_arrow state =
  let (id_opt, state) = parse_optional_id state in
  let (lbl, state) = expect_label state in
  let (start_pos, state) = parse_position state in
  let (end_pos, state) = parse_position state in
  let state = expect_token Lexer.SEMICOLON state in
  ({ Ast.id = id_opt; Ast.label = lbl; Ast.start_pos = start_pos; Ast.end_pos = end_pos }, state)

let rec parse_nodes state =
  match state.current with
  | Lexer.ID "node" ->
      let state = advance state in
      let (n, state) = parse_node state in
      let (ns, state) = parse_nodes state in
      (n :: ns, state)
  | _ -> ([], state)

let rec parse_arrows state =
  match state.current with
  | Lexer.ID "arrow" ->
      let state = advance state in
      let (a, state) = parse_arrow state in
      let (as_, state) = parse_arrows state in
      (a :: as_, state)
  | _ -> ([], state)

let parse_diagram tokens =
  let state = init_parser tokens in
  let (canvas, state) = parse_canvas state in
  let (nodes, state) = parse_nodes state in
  let (arrows, _) = parse_arrows state in
  {
    Ast.canvas = canvas;
    Ast.nodes = nodes;
    Ast.arrows = arrows;
  }

