open Parser
open Util

let u_to_str (ustr : Uchar.t array) =
  let buf = Buffer.create (Array.length ustr) in
  Array.iter (Buffer.add_utf_8_uchar buf) ustr;
  let b = Buffer.contents buf in
  (* p "%d:'%s'\n" (Array.length ustr) b; *)
  b


let tok_of_ident span (ident : string) =
  match ident with
  | "func" -> FUNC span
  | "var" -> VAR span
  | "if" -> IF span
  | "else" -> ELSE span
  | "const" -> CONST span
  | "return" -> RETURN span
  | _ -> IDENT (span, ident)


let lexeme buf = buf |> Sedlexing.lexeme |> u_to_str
let get_span buf = Span.of_positions (Sedlexing.lexing_positions buf)
let newline = [%sedlex.regexp? '\n' | '\r', '\n' | '\r']

let rec lex (buf : Sedlexing.lexbuf) =
  let ident = [%sedlex.regexp? Opt '#', (id_start | '_'), Star (id_continue | '_')] in
  let symbol = [%sedlex.regexp? '@', (id_start | '_'), Star (id_continue | '_')] in
  let decimal_number = [%sedlex.regexp? Plus '0' .. '9'] in
  let hexadecimal_number = [%sedlex.regexp? '0', ('x' | 'X'), Plus ascii_hex_digit] in
  let binary_number = [%sedlex.regexp? '0', ('b' | 'B'), Plus ('0' | '1')] in
  let tok =
    match%sedlex buf with
    | '\n' | '\r' | "\r\n" ->
      Sedlexing.new_line buf;
      lex buf
    | '"' -> string (Buffer.create 16) buf
    | white_space -> lex buf
    | '(' -> OPEN_PAREN (get_span buf)
    | ')' -> CLOSE_PAREN (get_span buf)
    | '{' -> OPEN_BRACE (get_span buf)
    | '}' -> CLOSE_BRACE (get_span buf)
    | ',' -> COMMA (get_span buf)
    | ':' -> COLON (get_span buf)
    | '.' -> DOT (get_span buf)
    | ';' -> SEMICOLON (get_span buf)
    | "==" -> EQEQ (get_span buf)
    | "//" -> IDIV (get_span buf)
    | '=' -> EQ (get_span buf)
    | '+' -> ADD (get_span buf)
    | '-' -> SUB (get_span buf)
    | '*' -> MUL (get_span buf)
    | '/' -> DIV (get_span buf)
    | '%' -> MOD (get_span buf)
    | '^' -> POW (get_span buf)
    | "<=" -> LEQ (get_span buf)
    | ">=" -> GEQ (get_span buf)
    | '<' -> LT (get_span buf)
    | '>' -> GT (get_span buf)
    | '!' -> NOT (get_span buf)
    | decimal_number | hexadecimal_number | binary_number ->
      INT (get_span buf, int_of_string (lexeme buf))
    | ident ->
      let ide = lexeme buf in
      let span = Span.of_positions (Sedlexing.lexing_positions buf) in
      tok_of_ident span ide
    | symbol ->
      let ide = lexeme buf in
      let span = Span.of_positions (Sedlexing.lexing_positions buf) in
      SYM (span, String.sub ide 1 (String.length ide - 1))
    | eof ->
      p "eof\n";
      EOF
    | any ->
      failwith (sp "unexpected character: '%s'" (buf |> Sedlexing.lexeme |> u_to_str))
    | _ -> failwith "lex: internal failure: reached unreachable state"
  in
  p
    "%s %s\n"
    (buf |> Sedlexing.lexing_positions |> Span.of_positions |> Span.to_string)
    (buf |> Sedlexing.lexeme |> u_to_str);
  tok


and string builder buf =
  match%sedlex buf with
  | '\\', '"' ->
    Buffer.add_char builder '"';
    string builder buf
  | newline ->
    Sedlexing.new_line buf;
    Buffer.add_string builder (lexeme buf);
    string builder buf
  | '"' -> STRING (get_span buf, Buffer.contents builder)
  | any ->
    Buffer.add_string builder (lexeme buf);
    string builder buf
  | _ -> failwith (sp "oh no %s" (lexeme buf))
