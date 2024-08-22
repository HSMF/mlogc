open Util

type typ =
  | TInt
  | TFlt
  | TStr
  | TBool
  | TNull
  | TBlock
  | TUnit
  | TContent

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | IDiv
  | Mod
  | Pow
  | Eq
  | Lt
  | Leq
  | Geq
  | Gt

type uop =
  | UMin
  | UNot

type expr =
  | BExpr of expr * bop * expr
  | UExpr of uop * expr
  | ENum of int
  | EString of string
  | EVar of string
  | ESym of string
  | EFuncCall of expr * expr list
  | EAccess of expr * string

type statement =
  | Decl of string * expr
  | Assign of expr * expr
  | If of expr * statement list * statement list
  | SFuncCall of expr * expr list
  | SRet

type func = string * (string * typ) list * statement list

type toplevel =
  | Func of func
  | ConstDecl of string * expr

let string_of_typ : typ -> string = function
  | TInt -> "int"
  | TFlt -> "float"
  | TStr -> "str"
  | TBool -> "bool"
  | TNull -> "null"
  | TBlock -> "block"
  | TUnit -> "unit"
  | TContent -> "content"


let typ_of_string = function
  | "int" -> TInt
  | "float" -> TFlt
  | "str" -> TStr
  | "bool" -> TBool
  | "null" -> TNull
  | "block" -> TBlock
  | "unit" -> TUnit
  | "content" -> TContent
  | s -> failwith @@ sp "unknown type %s" s


let string_of_bop : bop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | IDiv -> "//"
  | Mod -> "%"
  | Pow -> "^"
  | Eq -> "=="
  | Lt -> "<"
  | Leq -> "<="
  | Geq -> ">="
  | Gt -> ">"


let string_of_uop : uop -> string = function
  | UMin -> "-"
  | UNot -> "!"


let rec string_of_expr : expr -> string = function
  | BExpr (l, op, r) ->
    sp "(%s %s %s)" (string_of_expr l) (string_of_bop op) (string_of_expr r)
  | UExpr (op, e) -> sp "%s %s" (string_of_uop op) (string_of_expr e)
  | ENum n -> string_of_int n
  | ESym s -> "@" ^ s
  | EVar v -> v
  | EString s -> sp "\"%s\"" @@ String.escaped s
  | EFuncCall (f, args) -> sp "%s(%s)" (string_of_expr f) (sl string_of_expr ", " args)
  | EAccess (e, field) -> sp "%s.%s" (string_of_expr e) field


let rec string_of_statement' (ilevel : int) s =
  let str =
    match s with
    | Decl (lhs, rhs) -> sp "var %s = %s;" lhs (string_of_expr rhs)
    | Assign (lhs, rhs) -> sp "%s = %s;" (string_of_expr lhs) (string_of_expr rhs)
    | If (cond, yes, no) ->
      sp
        "if (%s) {\n%s%s} else {\n%s%s}"
        (string_of_expr cond)
        (string_of_block ilevel yes)
        (String.make (4 * ilevel) ' ')
        (string_of_block ilevel no)
        (String.make (4 * ilevel) ' ')
    | SFuncCall (f, args) -> sp "%s(%s);" (string_of_expr f) (sl string_of_expr ", " args)
  in
  String.make (ilevel * 4) ' ' ^ str


and string_of_block (ilevel : int) block =
  sl (fun x -> string_of_statement' (ilevel + 1) x ^ "\n") "" block


let rec string_of_statement : statement -> string = string_of_statement' 0

let string_of_func ((name, args, body) : func) =
  sp
    "func %s(%s) {\n%s}"
    name
    (sl (fun (arg, typ) -> sp "%s: %s" arg (string_of_typ typ)) ", " args)
    (string_of_block 0 body)


let string_of_toplevel = function
  | ConstDecl (s, e) -> sp "const %s = %s;" s (string_of_expr e)
  | Func f -> string_of_func f
