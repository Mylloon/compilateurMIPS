type reg =
  | Zero
  | V0
  | V1
  | A0
  | A1
  | A2
  | A3
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  | T8
  | T9
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | GP
  | SP
  | FP
  | RA

type label = string

type loc =
  | Lbl of label
  | Mem of reg * int

type instr =
  | Label of label
  | Li of reg * int
  | La of reg * loc
  | Sw of reg * loc
  | Lw of reg * loc
  | Move of reg * reg
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Mul of reg * reg * reg
  | Sub of reg * reg * reg
  | Div of reg * reg * reg
  | Rem of reg * reg * reg
  | Abs of reg * reg
  | Seq of reg * reg * reg
  | Sge of reg * reg * reg
  | Sgt of reg * reg * reg
  | Sle of reg * reg * reg
  | Slt of reg * reg * reg
  | Sne of reg * reg * reg
  | And of reg * reg * reg
  | Or of reg * reg * reg
  | Syscall
  | B of label
  | Beq of reg * reg * label
  | Beqz of reg * label
  | Jal of label
  | Jr of reg

type directive = Asciiz of string
type decl = label * directive

type asm =
  { text : instr list
  ; data : decl list
  }

module Syscall = struct
  let print_int = 1
  let print_str = 4
  let read_int = 5
  let read_str = 8
  let sbrk = 9
  let exit = 10
end

let fmt_reg = function
  | Zero -> "$zero"
  | V0 -> "$v0"
  | V1 -> "$v1"
  | A0 -> "$a0"
  | A1 -> "$a1"
  | A2 -> "$a2"
  | A3 -> "$a3"
  | T0 -> "$t0"
  | T1 -> "$t1"
  | T2 -> "$t2"
  | T3 -> "$t3"
  | T4 -> "$t4"
  | T5 -> "$t5"
  | T6 -> "$t6"
  | T7 -> "$t7"
  | T8 -> "$t8"
  | T9 -> "$t9"
  | S0 -> "$s0"
  | S1 -> "$s1"
  | S2 -> "$s2"
  | S3 -> "$s3"
  | S4 -> "$s4"
  | S5 -> "$s5"
  | S6 -> "$s6"
  | S7 -> "$s7"
  | GP -> "$gp"
  | SP -> "$sp"
  | FP -> "$fp"
  | RA -> "$ra"
;;

let fmt_loc = function
  | Lbl l -> l
  | Mem (r, o) -> Printf.sprintf "%d(%s)" o (fmt_reg r)
;;

let fmt_instr ?(indent = "  ") = function
  | Label l -> Printf.sprintf "%s:" l
  | Li (r, i) -> Printf.sprintf "%sli %s, %d" indent (fmt_reg r) i
  | La (r, a) -> Printf.sprintf "%sla %s, %s" indent (fmt_reg r) (fmt_loc a)
  | Sw (r, a) -> Printf.sprintf "%ssw %s, %s" indent (fmt_reg r) (fmt_loc a)
  | Lw (r, a) -> Printf.sprintf "%slw %s, %s" indent (fmt_reg r) (fmt_loc a)
  | Move (rd, rs) -> Printf.sprintf "%smove %s, %s" indent (fmt_reg rd) (fmt_reg rs)
  | Addi (rd, rs, i) ->
    Printf.sprintf "%saddi %s, %s, %d" indent (fmt_reg rd) (fmt_reg rs) i
  | Add (rd, rs, rt) ->
    Printf.sprintf "%sadd %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Mul (rd, rs, rt) ->
    Printf.sprintf "%smul %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sub (rd, rs, rt) ->
    Printf.sprintf "%ssub %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Div (rd, rs, rt) ->
    Printf.sprintf "%sdiv %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Rem (rd, rs, rt) ->
    Printf.sprintf "%srem %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Abs (rd, rs) -> Printf.sprintf "%sabs %s, %s" indent (fmt_reg rd) (fmt_reg rs)
  | Seq (rd, rs, rt) ->
    Printf.sprintf "%sseq %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sge (rd, rs, rt) ->
    Printf.sprintf "%ssge %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sgt (rd, rs, rt) ->
    Printf.sprintf "%ssgt %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sle (rd, rs, rt) ->
    Printf.sprintf "%ssle %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Slt (rd, rs, rt) ->
    Printf.sprintf "%sslt %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sne (rd, rs, rt) ->
    Printf.sprintf "%ssne %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | And (rd, rs, rt) ->
    Printf.sprintf "%sand %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Or (rd, rs, rt) ->
    Printf.sprintf "%sor %s, %s, %s" indent (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Syscall -> Printf.sprintf "%ssyscall" indent
  | B l -> Printf.sprintf "%sb %s" indent l
  | Beq (rs, rt, l) ->
    Printf.sprintf "%sbeq %s, %s, %s" indent (fmt_reg rs) (fmt_reg rt) l
  | Beqz (r, l) -> Printf.sprintf "%sbeqz %s, %s" indent (fmt_reg r) l
  | Jal l -> Printf.sprintf "%sjal %s" indent l
  | Jr r -> Printf.sprintf "%sjr %s" indent (fmt_reg r)
;;

let fmt_dir = function
  | Asciiz s -> Printf.sprintf ".asciiz \"%s\"" s
;;

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\n";
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text;
  Printf.fprintf
    oc
    "%s\n%s\n%s\n%s\n"
    (fmt_instr (Move (A0, V0)))
    (fmt_instr (Li (V0, 1)))
    (fmt_instr Syscall)
    (fmt_instr (Jr RA));
  Printf.fprintf oc "\n.data\n";
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
;;
