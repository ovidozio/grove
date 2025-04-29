(* lexer.ml *)
type token =
  | ID of string
  | LABEL of string
  | NUMBER of int
  | LPAREN
  | RPAREN
  | COMMA
  | SEMICOLON
  | EOF

exception LexError of string

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_id_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

let tokenize input =
  let len = String.length input in

  let rec skip_whitespace i =
    if i < len && is_whitespace input.[i] then
      skip_whitespace (i + 1)
    else
      i
  in

  let rec read_id i acc =
    if i < len && is_id_char input.[i] then
      read_id (i + 1) (acc ^ String.make 1 input.[i])
    else
      (ID acc, i)
  in

  let rec read_number i acc =
    if i < len && is_digit input.[i] then
      read_number (i + 1) (acc ^ String.make 1 input.[i])
    else
      (NUMBER (int_of_string acc), i)
  in

  let rec read_label i acc =
    if i >= len then
      raise (LexError "Unterminated label (missing closing quote)")
    else if input.[i] = '"' then
      (LABEL acc, i + 1)
    else
      read_label (i + 1) (acc ^ String.make 1 input.[i])
  in

  let rec aux i tokens =
    if i >= len then
      List.rev (EOF :: tokens)
    else
      let i = skip_whitespace i in
      if i >= len then
        List.rev (EOF :: tokens)
      else
        match input.[i] with
        | '(' -> aux (i + 1) (LPAREN :: tokens)
        | ')' -> aux (i + 1) (RPAREN :: tokens)
        | ',' -> aux (i + 1) (COMMA :: tokens)
        | ';' -> aux (i + 1) (SEMICOLON :: tokens)
        | '"' ->
            let (label, j) = read_label (i + 1) "" in
            aux j (label :: tokens)
        | '0'..'9' ->
            let (number, j) = read_number i "" in
            aux j (number :: tokens)
        | c when is_id_char c ->
            let (ident, j) = read_id i "" in
            aux j (ident :: tokens)
        | c ->
            let msg = Printf.sprintf "Unexpected character: '%c'" c in
            raise (LexError msg)
  in

  aux 0 []

