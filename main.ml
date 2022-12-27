open Errors

let () =
  if Array.length Sys.argv != 2
  then (
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1);
  let f = open_in Sys.argv.(1) in
  let buf = Lexing.from_channel f in
  try
    let parsed = Parser.prog Lexer.token buf in
    close_in f;
    (* Test.debug_parser Stdlib.stderr parsed; *)
    let ast = Semantics.analyze parsed in
    (* Test.debug_semantics Stdlib.stderr ast; *)
    let asm = Compiler.compile (Simplifier.simplify ast) in
    Mips.emit Stdlib.stdout asm
  with
  | LexerErrorC c ->
    err (Printf.sprintf "Unrecognized char \"%c\"" c) (Lexing.lexeme_start_p buf)
  | LexerErrorS s -> err (Printf.sprintf "%s" s) (Lexing.lexeme_start_p buf)
  | Parser.Error -> err "Syntax error" (Lexing.lexeme_start_p buf)
  | SemanticsError (msg, pos) -> err msg pos
;;
