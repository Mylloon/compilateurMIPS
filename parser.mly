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

%left Ladd Lsub
%left Lmul Ldiv

%start prog

%type <Ast.Syntax.block> prog

%%

prog:
  /* Fin de programme */
  | Lend { [] }

  /* instr; */
  | i = instr ; Lsc ; b = prog { i @ b }
;

instr:
  /* return x */
  | Lreturn ; e = expr { [ Return { expr = e ; pos = $startpos } ] }

  /* type v */
  | t = Ltype ; v = Lvar {
    [ Decl { name = v ; type_t = t ; pos = $startpos(t) } ]
  }

  /* type v = e */
  | t = Ltype ; v = Lvar ; Lassign ; e = expr
    { [ Decl   { name = v ; type_t = t ; pos = $startpos(t) }
    ; Assign { var = v ; expr = e ; pos = $startpos(v) } ]
    }

  /* v = e */
  | v = Lvar ; Lassign ; e = expr {
    [ Assign { var = v ; expr = e ; pos = $startpos($2) } ]
  }

expr:
  /* int */
  | n = Lint {
    Val { value = Int (n) ; pos = $startpos(n) }
  }

  /* bool */
  | b = Lbool {
    Val { value = Bool (b) ; pos = $startpos(b) }
  }

  /* Variable */
  | v = Lvar {
    Var { name = v ; pos = $startpos(v) }
  }

  /* e + e */
  | a = expr ; Ladd ; b = expr {
    Call { func = "%add" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e - e */
  | a = expr ; Lsub ; b = expr {
    Call { func = "%sub" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e * e */
  | a = expr ; Lmul ; b = expr {
    Call { func = "%mul" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e / e */
  | a = expr ; Ldiv ; b = expr {
    Call { func = "%div" ; args = [ a ; b ] ; pos = $startpos($2) }
  }
;
