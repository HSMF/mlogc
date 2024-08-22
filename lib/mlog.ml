open Util

type id = string
type lbl = string

type const =
  | CInt of int
  | CBool of bool
  | CStr of string
(* | CStr of string *)

and op =
  | OConst of const
  | OVar of string

and instr =
  (* Operations *)
  | Set of id * const
  | Operation of id * opcode * op * op
  (* Flow control *)
  | Wait of float
  | Stop
  | End
  | Ret (** ret is a pseudo instruction that emulates a return *)
  | Jump of lbl * cmp * id * const
  (* IO *)
  | Print of op
  (* Block control *)
  | PrintFlush of string
  (* Unit control *)
  | UBind of string
  | UControl of ucontrol

and ucontrol =
  (* ucontrol move x y _ _ _ *)
  | Move of op * op
  (* ucontrol stop _ _ _ _ _ *)
  | Stop
  (* ucontrol idle _ _ _ _ _ *)
  | Idle
  (* ucontrol approach x y radius _ _ *)
  | Approach of op * op * op
  (* ucontrol pathfind x y _ _ _ *)
  | Pathfind of op * op
  (* ucontrol autoPathfind _ _ _ _ _ *)
  | AutoPathfind
  (* ucontrol boost enable _ _ _ _ *)
  | Boost of op
  (* ucontrol target x y shoot _ _ *)
  | Target of op * op * op
  (* ucontrol targetp unit shoot _ _ _ *)
  | Targetp of op * op * op
  (* ucontrol itemDrop to amount _ _ _ *)
  | ItemDrop of op * op
  (* ucontrol itemTake from item amount _ _ *)
  | ItemTake of op * op * op
  (* ucontrol payDrop _ _ _ _ _ *)
  | PayDrop
  (* ucontrol payTake takeUnits _ _ _ _ *)
  | PayTake of op
  (* ucontrol payEnter _ _ _ _ _ *)
  | PayEnter
  (* ucontrol mine x y _ _ _ *)
  | Mine of op * op
  (* ucontrol flag value _ _ _ _ (numeric unit flag) *)
  | Flag of op
  (* ucontrol build x y block rotation config *)
  | Build of op * op * op * op * op
  (* ucontrol getBlock x y type building floor *)
  | GetBlock of op * op * op * op * op
  (* ucontrol within x y radius result _ *)
  | Within of op * op * op * op
  (* ucontrol unbind _ _ _ _ _ *)
  | Unbind

(*
   ucontrol move x y _ _ _
   ucontrol stop _ _ _ _ _
   ucontrol idle _ _ _ _ _
   ucontrol approach x y radius _ _
   ucontrol pathfind x y _ _ _
   ucontrol autoPathfind _ _ _ _ _
   ucontrol boost enable _ _ _ _
   ucontrol target x y shoot _ _
   ucontrol targetp unit shoot _ _ _
   ucontrol itemDrop to amount _ _ _
   ucontrol itemTake from item amount _ _
   ucontrol payDrop _ _ _ _ _
   ucontrol payTake takeUnits _ _ _ _
   ucontrol payEnter _ _ _ _ _
   ucontrol mine x y _ _ _
   ucontrol flag value _ _ _ _ (numeric unit flag)
   ucontrol build x y block rotation config
   ucontrol getBlock x y type building floor
   ucontrol within x y radius result _
   ucontrol unbind _ _ _ _ _ *)
and cmp =
  | Equal
  | NotEqual
  | LessThan
  | LessThanEq
  | GreaterThan
  | GreaterThanEq
  | StrictEqual
  | Always

and opcode =
  | MAdd
  | MSub
  | MMul
  | MDiv
  | MIdiv
  | MMod
  | MPow
  | MStrictEqual
  | MNotEqual
  | MLessThan
  | MLessThanEq
  | MGreaterThan
  | MGreaterThanEq
(*
   op add result a b
   op sub result a b
   op mul result a b
   op div result a b
   op idiv result a b
   op mod result a b
   op pow result a b
   op strictEqual result a b
   op notEqual result a b

   op lessThan result a b
   op lessThanEq result a b
   op greaterThan result a b
   op greaterThanEq result a b
*)

let ( +. ) a b =
  match a, b with
  | CInt a, CInt b -> CInt (a + b)
  | _ -> failwith "todo"


let ( -. ) a b =
  match a, b with
  | CInt a, CInt b -> CInt (a - b)
  | _ -> failwith "todo"


let ( *. ) a b =
  match a, b with
  | CInt a, CInt b -> CInt (a * b)
  | _ -> failwith "todo"


(* let ( /. ) a b = *)
(*   match a, b with *)
(*   | CInt a, CInt b -> CFloat (float_of_int a /. float_of_int b) *)
(*   | _ -> failwith "todo" *)

let ( //. ) a b =
  match a, b with
  | CInt a, CInt b -> CInt (a / b)
  | _ -> failwith "todo"


type cfg = instr list * (lbl * instr list) list

let op_true = OConst (CBool true)
let op_false = OConst (CBool false)
let izero = OConst (CInt 0)

let string_of_cmp = function
  | Equal -> "equal"
  | NotEqual -> "notEqual"
  | LessThan -> "lessThan"
  | LessThanEq -> "lessThanEq"
  | GreaterThan -> "greaterThan"
  | GreaterThanEq -> "greaterThanEq"
  | StrictEqual -> "strictEqual"
  | Always -> "always"


let string_of_opcode = function
  | MAdd -> "add"
  | MSub -> "sub"
  | MMul -> "mul"
  | MDiv -> "div"
  | MIdiv -> "idiv"
  | MMod -> "mod"
  | MPow -> "pow"
  | MStrictEqual -> "strictEqual"
  | MNotEqual -> "notEqual"
  | MLessThan -> "lessThan"
  | MLessThanEq -> "lessThanEq"
  | MGreaterThan -> "greaterThan"
  | MGreaterThanEq -> "greaterThanEq"


let string_of_const = function
  | CInt i -> string_of_int i
  | CBool b -> string_of_bool b
  | CStr s -> "\"" ^ String.escaped s ^ "\""


let string_of_op = function
  | OConst const -> string_of_const const
  | OVar v -> v


let strip_hash s = s |> String.split_on_char '#' |> flip List.nth 1

let string_of_ucontrol uc =
  let cmd, args =
    match uc with
    | Move (x, y) -> "move", [ x; y ]
    | Stop -> "stop", []
    | Idle -> "idle", []
    | Approach (x, y, rad) -> "approach", [ x; y; rad ]
    | Pathfind (x, y) -> "pathfind", [ x; y ]
    | AutoPathfind -> "autoPathfind", []
    | Boost enable -> "boost", [ enable ]
    | Target (x, y, s) -> "target", [ x; y; s ]
    | Targetp (x, y, s) -> "targetp", [ x; y; s ]
    | ItemDrop (to', amt) -> "itemDrop", [ to'; amt ]
    | ItemTake (fr, item, amt) -> "itemTake", [ fr; item; amt ]
    | PayDrop -> "payDrop", []
    | PayTake takeUnits -> "payTake", [ takeUnits ]
    | PayEnter -> "payEnter", []
    | Mine (x, y) -> "mine", [ x; y ]
    | Flag v -> "flag", [ v ]
    | Build (a, b, c, d, e) -> "build", [ a; b; c; d; e ]
    | GetBlock (a, b, c, d, e) -> "getBlock", [ a; b; c; d; e ]
    | Within (x, y, rad, res) -> "within", [ x; y; rad; res ]
    | Unbind -> "unbind", []
  in
  let args = args |> List.map string_of_op in
  let args = args @ List.init (5 - List.length args) (const "0") in
  sp "%s %s" cmd (sl id " " args)


let string_of_instr i =
  let s =
    match i with
    | Set (v, const) -> sp "set %s %s" v (string_of_const const)
    | Operation (target, code, l, r) ->
      sp "op %s %s %s %s" (string_of_opcode code) target (string_of_op l) (string_of_op r)
    | Wait f -> sp "wait %f" f
    | Stop -> "stop"
    | End -> "end"
    | Ret -> failwith "todo"
    | Jump (target, cmp, var, const) ->
      sp "jump %s %s %s %s" target (string_of_cmp cmp) var (string_of_const const)
    | Print o -> sp "print %s" (string_of_op o)
    | PrintFlush where -> sp "printflush %s" (strip_hash where)
    | UBind s -> sp "ubind %s" s
    | UControl uc -> "ucontrol " ^ string_of_ucontrol uc
  in
  s ^ "\n"


let string_of_instrs : instr list -> string = sl string_of_instr ""

let string_of_cfg ((start, labelled) : cfg) =
  string_of_instrs start
  ^ sl (fun (lab, body) -> sp "%s:\n%s" lab (string_of_instrs body)) "" labelled

(*
   print "frog"
printflush message1
ubind @poly

op add result a b
op sub result a b
op mul result a b
op div result a b
op idiv result a b
op mod result a b
op pow result a b
op strictEqual result a b
op notEqual result a b
op sin result a b
op sin result a b



ubind @emanate
ucontrol move x y _ _ _
ucontrol stop _ _ _ _ _
ucontrol idle _ _ _ _ _
ucontrol approach x y radius _ _
ucontrol pathfind x y _ _ _
ucontrol autoPathfind _ _ _ _ _
ucontrol boost enable _ _ _ _
ucontrol target x y shoot _ _
ucontrol targetp unit shoot _ _ _
ucontrol itemDrop to amount _ _ _
ucontrol itemTake from item amount _ _
ucontrol payDrop _ _ _ _ _
ucontrol payTake takeUnits _ _ _ _
ucontrol payEnter _ _ _ _ _
ucontrol mine x y _ _ _
ucontrol flag value _ _ _ _ (numeric unit flag)
ucontrol build x y block rotation config
ucontrol getBlock x y type building floor
ucontrol within x y radius result _
ucontrol unbind _ _ _ _ _
*)
