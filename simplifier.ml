open Ast

let collect_constant_strings code =
  let counter = ref 0 in
  let ccs_value = function
    | V1.Void -> V2.Void, []
    | V1.Bool b -> V2.Bool b, []
    | V1.Int n -> V2.Int n, []
    | V1.Str s ->
      incr counter;
      let lbl = "str" ^ string_of_int !counter in
      V2.Data lbl, [ lbl, Mips.Asciiz s ]
  in
  let rec ccs_expr = function
    | IR1.Val v ->
      let v2, cs = ccs_value v in
      IR2.Val v2, cs
    | IR1.Var v -> IR2.Var v, []
    | IR1.Call (fn, args) ->
      let a2 = List.map ccs_expr args in
      IR2.Call (fn, List.map fst a2), List.flatten (List.map snd a2)
  in
  let ccs_instr = function
    | IR1.Decl v -> IR2.Decl v, []
    | IR1.Assign (lv, e) ->
      let e2, cs = ccs_expr e in
      IR2.Assign (lv, e2), cs
    | IR1.Do e ->
      let e2, cs = ccs_expr e in
      IR2.Do e2, cs
    | IR1.Return e ->
      let e2, cs = ccs_expr e in
      IR2.Do e2, cs
  in
  let rec ccs_block acc_b acc_cs = function
    | i :: b ->
      let i2, cs = ccs_instr i in
      ccs_block (i2 :: acc_b) (cs @ acc_cs) b
    | [] -> List.rev acc_b, acc_cs
  in
  let ccs_def (IR1.Func (name, args, body)) =
    let body2, cs = ccs_block [] [] body in
    IR2.Func (name, args, body2), cs
  in
  let code2 = List.map ccs_def code in
  List.map fst code2, List.flatten (List.map snd code2)
;;

let simplify ir = collect_constant_strings ir
