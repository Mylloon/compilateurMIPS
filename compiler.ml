open Ast.IR2
open Ast.V2
open Mips
open Baselib

type info =
  { asm : instr list
  ; env : loc Env.t
  ; fpo : int (* FP offset *)
  ; cnt : int (* Counter *)
  ; ret : string (* Return *)
  }

let puf = "f_" (* prefix user function *)
let cnt_inline = ref (-1)

let compile_value = function
  | Void -> [ Li (V0, 0) ]
  | Int n -> [ Li (V0, n) ]
  | Bool b -> [ Li (V0, if b then 1 else 0) ]
  | Data l -> [ La (V0, Lbl l) ]
  | Ptr p -> [ Li (V0, p) ]
;;

let rec compile_expr env = function
  | Val v -> compile_value v
  | Var v -> [ Lw (V0, Env.find v env) ]
  | Call (f, args) ->
    let ca =
      List.map
        (fun a -> compile_expr env a @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
        args
    in
    let uniq =
      if List.mem f Baselib.builtins_special
      then (
        incr cnt_inline;
        string_of_int !cnt_inline)
      else ""
    in
    List.flatten ca
    @
    if Env.mem f (Baselib.builtins uniq)
    then Env.find f (Baselib.builtins uniq)
    else [ Jal (puf ^ f); Addi (SP, SP, 4 * List.length args) ]
;;

let rec compile_instr info = function
  | Decl v ->
    { info with env = Env.add v (Mem (FP, -info.fpo)) info.env; fpo = info.fpo + 4 }
  | Assign (lv, e) ->
    { info with
      asm =
        (info.asm
        @ compile_expr info.env e
        @
        match lv with
        | Name v -> [ Sw (V0, Env.find v info.env) ]
        | Addr e ->
          [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ]
          @ compile_expr info.env e
          @ [ Lw (T0, Mem (SP, 0)); Addi (SP, SP, 4); Sw (T0, Mem (V0, 0)) ])
    }
  | Cond (e, ib, eb) ->
    let uniq = string_of_int info.cnt in
    let cib = compile_block { info with asm = []; cnt = info.cnt + 1 } ib in
    let ceb = compile_block { info with asm = []; cnt = cib.cnt } eb in
    { info with
      asm =
        info.asm
        @ compile_expr info.env e
        @ [ Beqz (V0, "else" ^ uniq) ]
        @ cib.asm
        @ [ B ("endif" ^ uniq); Label ("else" ^ uniq) ]
        @ ceb.asm
        @ [ Label ("endif" ^ uniq) ]
    ; cnt = ceb.cnt
    }
  | Loop (e, b) ->
    let uniq = string_of_int info.cnt in
    let cb = compile_block { info with asm = []; cnt = info.cnt + 1 } b in
    { info with
      asm =
        info.asm
        @ [ Label ("while" ^ uniq) ]
        @ compile_expr info.env e
        @ [ Beqz (V0, "endwhile" ^ uniq) ]
        @ cb.asm
        @ [ J ("while" ^ uniq) ]
        @ [ Label ("endwhile" ^ uniq) ]
    ; cnt = cb.cnt
    }
  | Do e -> { info with asm = info.asm @ compile_expr info.env e }
  | Return e -> { info with asm = info.asm @ compile_expr info.env e @ [ B info.ret ] }

and compile_block info = function
  | [] -> info
  | i :: b -> compile_block (compile_instr info i) b
;;

let compile_def (Func (name, args, body)) counter =
  let compiled =
    compile_block
      { asm = []
      ; env =
          List.fold_left
            (fun env (arg, addr) -> Env.add arg addr env)
            Env.empty
            (List.mapi (fun i a -> a, Mem (FP, 4 * (i + 1))) (List.rev args))
      ; fpo = 8
      ; cnt = counter + 1
      ; ret = "ret" ^ string_of_int counter
      }
      body
  in
  let lbl_puf = if name = "main" then "" else puf in
  ( compiled.cnt
  , [ Label (lbl_puf ^ name)
    ; Addi (SP, SP, -compiled.fpo)
    ; Sw (RA, Mem (SP, compiled.fpo - 4))
    ; Sw (FP, Mem (SP, compiled.fpo - 8))
    ; Addi (FP, SP, compiled.fpo - 4)
    ]
    @ compiled.asm
    @ [ Label compiled.ret
      ; Addi (SP, SP, compiled.fpo)
      ; Lw (RA, Mem (FP, 0))
      ; Lw (FP, Mem (FP, -4))
      ; Jr RA
      ] )
;;

let rec compile_prog counter = function
  | [] -> []
  | d :: r ->
    let new_counter, cd = compile_def d counter in
    cd @ compile_prog new_counter r
;;

let compile (ir, data) =
  let asm = compile_prog 0 ir in
  { text = asm; data }
;;
