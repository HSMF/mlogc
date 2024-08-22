open! Mlogc.Util
open! Mlogc

let () = print_newline ()
let ep = Printf.eprintf
let p = Printf.printf

let file, ofile =
  let file = ref None in
  let ofile = ref None in
  let options =
    [ ( "-o"
      , Arg.String (fun s -> ofile := Some s)
      , "output file to write. '-' writes to stdout" )
    ]
  in
  let help_msg = "mlogc [options] <infile>" in
  Arg.parse options (fun s -> file := Some s) help_msg;
  let file =
    match !file with
    | Some x -> x
    | None ->
      ep "missing positional argument file\n";
      Arg.usage options help_msg;
      exit 1
  in
  file, !ofile |> Option.unwrap_or "-"


let parse_file name =
  let f = open_in name in
  let lexbuf = Sedlexing.Utf8.from_channel f in
  Sedlexing.set_filename lexbuf name;
  let lexer = Sedlexing.with_tokenizer Mlogc.Lexer.lex lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Mlogc.Parser.prog in
  let o =
    try parser lexer with
    | Parser.Error ->
      failwith
        (sp
           "parse error %s"
           (Sedlexing.lexing_positions lexbuf |> Span.of_positions |> Span.to_string))
  in
  close_in f;
  o


let () =
  let ast = parse_file file in
  p "%s" (sl Ast.string_of_toplevel "\n\n" ast);
  p "\n\n";
  let cfg = Compile.compile ast in
  p "\n\n";
  p "%s\n" @@ Mlog.string_of_cfg cfg;
  p "\n\n";
  ()
