%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token Lend

%start prog

%type <Ast.Syntax.expr> prog

%%

prog:
| e = expr; Lend { e }
;

expr:
| n = Lint {
  Int { value = n ; pos = $startpos(n) }
}
;
