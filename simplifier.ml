open Ast
open Baselib

let collect_constant_strings code =
  let counter = ref 0 in
  let env = ref Env.empty in
  let ccs_value = function
    | V1.Void -> V2.Void, []
    | V1.Bool b -> V2.Bool b, []
    | V1.Int n -> V2.Int n, []
    | V1.Str s ->
      (match Env.find_opt s !env with
      | Some lbl -> V2.Data lbl, [ lbl, Mips.Asciiz s ]
      | None ->
        let lbl = "str" ^ string_of_int !counter in
        env := Env.add s lbl !env;
        incr counter;
        V2.Data lbl, [ lbl, Mips.Asciiz s ])
  in
  let rec ccs_expr = function
    | IR1.Val v ->
      let v2, ccs = ccs_value v in
      IR2.Val v2, ccs
    | IR1.Var v -> IR2.Var v, []
    | IR1.Call (fn, args) ->
      let args2 = List.map ccs_expr args in
      IR2.Call (fn, List.map fst args2), List.flatten (List.map snd args2)
  in
  let rec ccs_instr = function
    | IR1.Decl v -> IR2.Decl v, []
    | IR1.Assign (lv, e) ->
      let e2, ccs = ccs_expr e in
      IR2.Assign (lv, e2), ccs
    | IR1.Do e ->
      let e2, ccs = ccs_expr e in
      IR2.Do e2, ccs
    | IR1.Cond (e, ib, eb) ->
      let e2, ccs = ccs_expr e in
      let ib2, ccs2 = ccs_block ib in
      let eb2, ccs3 = ccs_block eb in
      IR2.Cond (e2, ib2, eb2), List.flatten [ ccs; ccs2; ccs3 ]
    | IR1.Loop (e, b) ->
      let e2, ccs = ccs_expr e in
      let b2, ccs2 = ccs_block b in
      IR2.Loop (e2, b2), List.flatten [ ccs; ccs2 ]
    | IR1.Return e ->
      let e2, ccs = ccs_expr e in
      IR2.Return e2, ccs
  and ccs_block = function
    | [] -> [], []
    | i :: s ->
      let i2, ccs_i = ccs_instr i in
      let b, ccs_r = ccs_block s in
      i2 :: b, List.flatten [ ccs_i; ccs_r ]
  in
  let ccs_def (IR1.Func (name, args, body)) =
    let body2, ccs = ccs_block body in
    IR2.Func (name, args, body2), ccs
  in
  let code2 = List.map ccs_def code in
  ( List.map fst code2
  , List.fold_left
      (fun list el -> if List.mem el list then list else el :: list)
      []
      (List.flatten (List.map snd code2)) )
;;

let simplify ir = collect_constant_strings ir
