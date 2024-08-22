open Ast
open Util

type elt =
  | I of Mlog.instr
  | T of Mlog.op * Mlog.lbl * Mlog.lbl
  | H of Mlog.instr
  | L of Mlog.lbl

type stream = elt list
type ctxt = (string * Mlog.op) list

let c_add (ctxt : ctxt) name info = (name, info) :: ctxt

let c_get (ctxt : ctxt) name =
  try List.assoc name ctxt with
  | Not_found ->
    ep "%s\n" name;
    raise Not_found


let as_var = function
  | Mlog.OVar v -> v
  | _ -> failwith "not a variable"


let p_stream s =
  sl
    (function
      | I i -> "I " ^ Mlog.string_of_instr i
      | T (op, yes, no) -> sp "T %s %s %s\n" (Mlog.string_of_op op) yes no
      | H i -> "H " ^ Mlog.string_of_instr i
      | L l -> "L " ^ l)
    ""
    s
  |> p "%s\n"


let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x

let cfg_of_stream (stream : stream) : Mlog.cfg =
  let open Mlog in
  let a, b =
    List.fold_left
      (fun (cur, completed) elt ->
        match elt with
        | I i -> i :: cur, completed
        | T (OConst (Mlog.CBool true), yes, _) ->
          [ Jump (yes, Always, "r", CBool true) ], completed
        | T (OConst (Mlog.CBool false), _, no) ->
          [ Jump (no, Always, "r", CBool true) ], completed
        | T (OConst _, _, _) -> failwith "weird const"
        | T (OVar v, yes, no) ->
          [ Jump (yes, Equal, v, CInt 1); Jump (no, Equal, v, CInt 0) ], completed
        | H _ -> failwith "todo cfg_of_stream case H _"
        | L lbl -> [], (lbl, cur) :: completed)
      ([], [])
      stream
  in
  a, b


let fresh =
  let c = ref 0 in
  fun s ->
    incr c;
    sp "%s%d" s !c


let new_id () = fresh "vv"

let rec find_and_remove p acc lst =
  match lst with
  | [] -> raise Not_found
  | x :: xs when p x -> x, acc @ xs
  | x :: xs -> find_and_remove p (x :: acc) xs


let rec const_eval =
  let open Mlog in
  function
  | BExpr (l, Add, r) -> const_eval l +. const_eval r
  | BExpr (l, Sub, r) -> const_eval l -. const_eval r
  | BExpr (l, Mul, r) -> const_eval l *. const_eval r
  | BExpr (l, IDiv, r) -> const_eval l //. const_eval r
  | BExpr (_, _, _) -> failwith "todo"
  | UExpr (_, _) -> failwith "todo"
  | ENum i -> CInt i
  | EString s -> CStr s
  | EVar _ -> failwith "no"
  | ESym _ -> failwith "no"
  | EFuncCall (_, _) -> failwith "no"
  | EAccess (_, _) -> failwith "no"


let compile_opcode =
  let open Mlog in
  function
  | Add -> MAdd
  | Sub -> MSub
  | Mul -> MMul
  | Div -> MDiv
  | IDiv -> MIdiv
  | Mod -> MMod
  | Pow -> MPow
  | Eq -> MStrictEqual
  | Lt -> MLessThan
  | Leq -> MLessThanEq
  | Geq -> MGreaterThanEq
  | Gt -> MGreaterThan


let rec compile_expr ctxt (e : expr) : Mlog.op * stream =
  let open Mlog in
  match e with
  | BExpr (l, op, r) -> begin
    let op_l, sl = compile_expr ctxt l in
    let op_r, sr = compile_expr ctxt r in
    let res = new_id () in
    OVar res, sl >@ sr >:: I (Operation (res, compile_opcode op, op_l, op_r))
  end
  | UExpr (_, _) -> failwith "todo compile_expr case UExpr (_, _)"
  | ENum i -> OConst (CInt i), []
  | EString s -> OConst (CStr s), []
  | EVar v -> c_get ctxt v, []
  | ESym _ -> failwith "todo compile_expr case ESym _"
  | EFuncCall (EVar "flush", [ EVar msg ]) -> izero, [] >:: I (PrintFlush msg)
  | EFuncCall (EVar "ubind", [ ESym sym ]) -> izero, [] >:: I (UBind ("@" ^ sym))
  | EFuncCall (EAccess (ESym "unit", "move"), [ x; y ]) ->
    let opx, cx = compile_expr ctxt x in
    let opy, cy = compile_expr ctxt y in
    izero, cx >@ cy >:: I (UControl (Move (opx, opy)))
  | EFuncCall (EAccess (ESym "unit", "is_within"), [ x; y; radius ]) ->
    let opx, cx = compile_expr ctxt x in
    let opy, cy = compile_expr ctxt y in
    let opr, cr = compile_expr ctxt radius in
    let res = new_id () in
    OVar res, cx >@ cy >@ cr >:: I (UControl (Within (opx, opy, opr, OVar res)))
  | EFuncCall (EAccess (ESym "unit", "build"), [ x; y; ESym block; rotation; config ]) ->
    let opx, cx = compile_expr ctxt x in
    let opy, cy = compile_expr ctxt y in
    let opr, cr = compile_expr ctxt rotation in
    let opc, cc = compile_expr ctxt config in
    let res = new_id () in
    ( OVar res
    , cx
      >@ cy
      >@ cr
      >@ cc
      >:: I (UControl (Build (opx, opy, OVar ("@" ^ block), opr, opc))) )
  | EFuncCall (EVar "wait_ms", [ ENum i ]) ->
    OConst (CInt 0), [] >:: I (Wait Stdlib.(float_of_int i /. 1000.0))
  | EFuncCall (EVar "print", args) -> begin
    let s' =
      List.fold_left
        (fun s arg ->
          let op, s' = compile_expr ctxt arg in
          s >@ s' >:: I (Print op))
        []
        args
    in
    OConst (CInt 0), s'
  end
  | EFuncCall (EVar _fn, _args) ->
    failwith "todo compile_expr case EFuncCall (EVar fn, args)"
  | EFuncCall (_, _) -> failwith "function operand type not supported"
  | EAccess (_, _) -> failwith "todo compile_expr case EAccess (_, _)"


let rec compile_statement fn ctxt (s : statement) : ctxt * stream =
  match s with
  | Decl (v, ex) -> begin
    let ex_loc, s' = compile_expr ctxt ex in
    match ex_loc with
    | OVar _ -> c_add ctxt v ex_loc, s'
    | OConst c ->
      let loc = new_id () in
      c_add ctxt v (OVar loc), s' >:: I (Mlog.Set (loc, c))
  end
  | Assign (EVar v, ex) -> begin
    let ex_loc, s' = compile_expr ctxt ex in
    let loc = as_var (c_get ctxt v) in
    match ex_loc with
    | OVar v' when v = v' -> ctxt, s'
    | OVar _ -> ctxt, s' >:: I (Operation (loc, Mlog.MAdd, ex_loc, Mlog.izero))
    | OConst c -> ctxt, s' >:: I (Mlog.Set (loc, c))
  end
  | Assign (_, _) -> failwith "todo compile_statement case Assign (_, _)"
  | If (cond, yes, no) -> begin
    let condop, condc = compile_expr ctxt cond in
    let after_label = fresh "after" in
    let yes_label = fresh "if" in
    let no_label = fresh "else" in
    let _, yes_s = compile_block fn ctxt yes in
    let _, no_s = compile_block fn ctxt no in
    ( ctxt
    , condc
      >:: T (condop, yes_label, no_label)
      >:: L yes_label
      >@ yes_s
      >:: T (Mlog.op_true, after_label, after_label)
      >:: L no_label
      >@ no_s
      >:: T (Mlog.op_true, after_label, after_label)
      >:: L after_label )
  end
  | SFuncCall (f, args) ->
    let _, s = compile_expr ctxt (EFuncCall (f, args)) in
    ctxt, s
  | SRet -> failwith "todo compile_statement case SRet"


and compile_block (fn : string) ctxt body : ctxt * stream =
  List.fold_left
    (fun (c, stream) stmt ->
      p "%s\n" @@ sl fst ", " c;
      let c', s' = compile_statement fn c stmt in
      c', stream >@ s')
    (ctxt, [])
    body


let compile_func ctxt ((name, _args, body) : func) : ctxt * stream =
  let c, s = compile_block name ctxt body in
  c, [] >:: L name >@ s


let compile (program : Ast.toplevel list) : Mlog.cfg =
  let constants, funcs =
    List.fold_left
      (fun (const, fn) item ->
        match item with
        | ConstDecl (name, c) -> (name, c) :: const, fn
        | Func func -> const, func :: fn)
      ([], [])
      program
  in
  let constants = List.map (fun (n, v) -> n, const_eval v) constants in
  List.iter (fun (c, v) -> p "%s = %s\n" c (Mlog.string_of_const v)) constants;
  let setup_func, funcs = find_and_remove (fun (n, _, _) -> n = "setup") [] funcs in
  let main_func, funcs = find_and_remove (fun (n, _, _) -> n = "loop") [] funcs in
  let ctxt = List.map (fun (c, v) -> c, Mlog.OConst v) constants in
  let c, setup_c = compile_func ctxt setup_func in
  let _, main_c = compile_func c main_func in
  let main_c = main_c >:: T (Mlog.op_true, "loop", "loop") in
  let stream = setup_c >:: T (Mlog.op_true, "loop", "loop") >@ main_c in
  p_stream stream;
  cfg_of_stream stream
