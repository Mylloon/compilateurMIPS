open Ast.IR
open Mips
module Env = Map.Make (String)

type info =
  { asm : instr list
  ; env : loc Env.t
  ; fpo : int (* FP offset *)
  ; cnt : int (* Counter *)
  ; ret : string (* Return *)
  }

let compile_value = function
  | Int n -> [ Li (V0, n) ]
  | Bool b -> [ Li (V0, if b then 1 else 0) ]
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
    List.flatten ca @ Env.find f Baselib.builtins
;;

let compile_instr info = function
  | Decl v ->
    { info with env = Env.add v (Mem (FP, -info.fpo)) info.env; fpo = info.fpo + 4 }
  | Assign (v, e) ->
    { info with
      asm = info.asm @ compile_expr info.env e @ [ Sw (V0, Env.find v info.env) ]
    }
  | Return e -> { info with asm = info.asm @ compile_expr info.env e @ [ B info.ret ] }
;;

let rec compile_block info = function
  | [] -> info
  | i :: b -> compile_block (compile_instr info i) b
;;

let compile_body body counter =
  let compiled =
    compile_block
      { asm = []
      ; env = Env.empty
      ; fpo = 8
      ; cnt = counter + 1
      ; ret = "ret" ^ string_of_int counter
      }
      body
  in
  [ Addi (SP, SP, -compiled.fpo)
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
    ]
;;

let compile ir =
  let asm = compile_body ir 0 in
  { text = asm; data = [] }
;;
