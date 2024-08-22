open Util

type loc = int * int
type span = string * loc * loc

let string_of_loc (line, col) =
  if line <> -1 then sp "%d:%d" line col else "<no loc>"

let no_loc = (-1, -1)

let loc_of_position (pos : Lexing.position) =
  (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let to_string (file, a, b) =
  sp "%s:%s-%s" file (string_of_loc a) (string_of_loc b)

let of_positions ((a : Lexing.position), (b : Lexing.position)) : span =
  (a.pos_fname, loc_of_position a, loc_of_position b)

let no_span = ("", no_loc, no_loc)
