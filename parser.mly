%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool

%token <Ast.type_t> Ltype

%token <string> Lvar

%token Lend Lassign Lsc

%start prog

%type <Ast.Syntax.block> prog

%%

prog:
  | Lend { [] }
  | i = instr; Lsc; b = prog { i @ b }
;

instr:
  | t = Ltype; v = Lvar {
    [ Decl { name = v; type_t = t; pos = $startpos(t) } ]
  }
  | t = Ltype; v = Lvar; Lassign; e = expr
    { [ Decl   { name = v; type_t = t; pos = $startpos(t) }
    ; Assign { var = v; expr = e; pos = $startpos(v) } ]
    }
  | v = Lvar; Lassign; e = expr
    { [ Assign { var = v; expr = e; pos = $startpos(v) } ]
    }

expr:
  | n = Lint {
    Val { value = Int (n) ; pos = $startpos(n) }
  }
  | b = Lbool {
    Val { value = Bool (b) ; pos = $startpos(b) }
  }
  | v = Lvar {
    Var { name = v ; pos = $startpos(v) }
  }
;
