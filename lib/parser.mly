%{
open Ast
open Util

let fst (a, _) = a
let snd (_, a) = a

let rec construct_if_else_tree last = function
  | [] -> last
  | (cond, body) :: more -> [ If (cond, body, construct_if_else_tree last more) ]
%}

%token <Span.span * int> INT
%token <Span.span * string> STRING
%token <Span.span * string> IDENT
%token <Span.span * string> SYM
%token EOF

%token <Span.span> FUNC
%token <Span.span> VAR
%token <Span.span> IF
%token <Span.span> ELSE
%token <Span.span> CONST
%token <Span.span> RETURN

%token <Span.span> OPEN_PAREN
%token <Span.span> CLOSE_PAREN
%token <Span.span> OPEN_BRACE
%token <Span.span> CLOSE_BRACE
%token <Span.span> COMMA
%token <Span.span> COLON
%token <Span.span> EQ
%token <Span.span> SEMICOLON
%token <Span.span> DOT

%token <Span.span> ADD
%token <Span.span> SUB
%token <Span.span> MUL
%token <Span.span> DIV
%token <Span.span> IDIV
%token <Span.span> MOD
%token <Span.span> POW
%token <Span.span> EQEQ
%token <Span.span> LT
%token <Span.span> LEQ
%token <Span.span> GEQ
%token <Span.span> GT
%token <Span.span> NOT

%start <toplevel list> prog

%left EQEQ LT GT LEQ GEQ 
%left ADD SUB
%left MUL DIV IDIV MOD
%left POW


%%

prog:
  | x=list(top_level) EOF { x }

top_level:
  | f=func { Func f }
  | c=const { c }

func:
  | FUNC name=IDENT OPEN_PAREN args=separated_list(COMMA, func_arg) option(COMMA) CLOSE_PAREN body=block {  (snd name, args, body) }

const:
  | CONST name=IDENT EQ e=c_expr SEMICOLON { ConstDecl (snd name, e) }

func_arg:
  | x=IDENT COLON t=typ { snd x, t }

typ:
| t=IDENT { typ_of_string (snd t)  }

block:
  | OPEN_BRACE statements=list(statement) CLOSE_BRACE { statements }

statement:
  | VAR var=IDENT EQ e=expr SEMICOLON {Decl (snd var, e)}
  | var=expr EQ e=expr SEMICOLON { Assign (var, e) }
  | fn=fcall SEMICOLON { let (fn,args) = fn in SFuncCall(fn, args) }
  (* | IF cond=expr body=block chain=option(list(elseif)) { If(cond, body, construct_if_else_tree [] (Option.unwrap_or [] chain)) } *)
  (* | IF cond=expr body=block chain=option(list(elseif)) ELSE otherwise=block { If(cond, body, construct_if_else_tree otherwise (Option.unwrap_or [] chain)) } *)
  | v=iff { v }
  | RETURN SEMICOLON { SRet }

iff:
  | IF cond=expr body=block { If (cond, body, [] )}
  | IF cond=expr body=block chain=elseif { If(cond, body, construct_if_else_tree [] chain) }
  | IF cond=expr body=block chain=elseif ELSE final=block  { If(cond, body, construct_if_else_tree final chain) }
  | IF cond=expr body=block ELSE final=block  { If(cond, body, construct_if_else_tree final []) }

elseif:
  | ELSE IF cond=expr body=block more=elseif { (cond, body) :: more }
  | {[]}

expr:
  | e=c_expr { e }
  | l=expr b=bop r=expr { BExpr(l, b, r) }
  | u=uop e=expr { UExpr(u, e) }
  | fn=fcall { let (fn,args) = fn in EFuncCall(fn, args) }
  | OPEN_PAREN e=expr CLOSE_PAREN { e }
  | e=expr DOT field=IDENT { EAccess(e, snd field) }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | IDIV { IDiv }
  | MOD { Mod }
  | POW { Pow }
  | EQEQ { Eq }
  | LT { Lt }
  | LEQ { Leq }
  | GEQ { Geq }
  | GT { Gt }

%inline uop:
  | SUB { UMin }
  | NOT { UNot }


c_expr:
  | i=INT { ENum (snd i) }
  | var=IDENT { EVar(snd var) }
  | s=SYM { ESym(snd s) }
  | s=STRING { EString (snd s) }


fcall:
  | fn=expr OPEN_PAREN args=separated_list(COMMA, expr) option(COMMA) CLOSE_PAREN {fn, args }
