type reg =
  | V0

type label = string

type instr =
  | Li    of reg * int

type directive =
  | Asciiz of string

type decl = label * directive

type asm = { text: instr list ; data: decl list }

let ps = Printf.sprintf (* alias raccourci *)

let fmt_reg = function
  | V0   -> "$v0"

let fmt_instr = function
  | Li (r, i) -> ps "  li %s, %d" (fmt_reg r) i

let fmt_dir = function
  | Asciiz (s) -> ps ".asciiz \"%s\"" s

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\nmain:\n" ;
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
  Printf.fprintf oc "  move $a0, $v0\n  li $v0, 1\n  syscall\n  jr $ra\n" ;
  Printf.fprintf oc "\n.data\n" ;
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
