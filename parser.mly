%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <Ast.type_t> Ltype
%token <string> Lvar
%token Lend Lassign Lsc Lreturn
%token Ladd Lsub Lmul Ldiv

%left Ladd Lsub Lmul Ldiv

%start prog

%type <Ast.Syntax.block> prog

%%

prog:
  | Lend { [] }
  | i = instr ; Lsc ; b = prog { i @ b }
;

instr:
  | Lreturn ; e = expr { [ Return { expr = e ; pos = $startpos } ] }
  | t = Ltype ; v = Lvar {
    [ Decl { name = v ; type_t = t ; pos = $startpos(t) } ]
  }
  | t = Ltype ; v = Lvar ; Lassign ; e = expr
    { [ Decl   { name = v ; type_t = t ; pos = $startpos(t) }
    ; Assign { var = v ; expr = e ; pos = $startpos(v) } ]
    }
  | v = Lvar ; Lassign ; e = expr
    { [ Assign { var = v ; expr = e ; pos = $startpos($2) } ]
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
  | a = expr ; Ladd ; b = expr {
    Call { func = "%add" ; args = [ a ; b ] ; pos = $startpos($2) }
  }
  | a = expr ; Lsub ; b = expr {
    Call { func = "%sub" ; args = [ a ; b ] ; pos = $startpos($2) }
  }
  | a = expr ; Lmul ; b = expr {
    Call { func = "%mul" ; args = [ a ; b ] ; pos = $startpos($2) }
  }
  | a = expr ; Ldiv ; b = expr {
    Call { func = "%div" ; args = [ a ; b ] ; pos = $startpos($2) }
  }
;
