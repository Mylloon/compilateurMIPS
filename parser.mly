%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <Ast.type_t> Ltype
%token <string> Lvar
%token Lend Lassign Lsc Lreturn
%token Lbracedeb Lbracefin
%token Lpardeb Lparfin Lcomma
%token Ladd Lsub Lmul Ldiv

%left Ladd Lsub Lmul Ldiv

%start prog

%type <Ast.Syntax.block> block
%type <Ast.Syntax.prog> prog
%type <Ast.Syntax.ident list> args_ident
%type <Ast.Syntax.expr list> args_expr

%%

prog:
  /* Liste des définitions de fonction */
  | i = def ; b = prog { i @ b }
  /* Fin de programme */
  | Lend { [] }

def:
  /* Définition fonction : type fonction (args) block */
  | t = Ltype
  ; f = Lvar
  ; a = args_ident
  ; b = block {
    [ Func { func = f ; type_t = t ; args = a ; code = b ; pos = $startpos(f) } ]
  }

  /* Définition fonction : type fonction () block */
  | t = Ltype
  ; f = Lvar
  ; Lpardeb
  ; Lparfin
  ; b = block {
    [ Func { func = f ; type_t = t ; args = [] ; code = b ; pos = $startpos(f) } ]
  }

args_ident:
  /* ( */
  | Lpardeb ; s = args_ident { s }

  /* a, ... */
  | a = arg_ident ; Lcomma ; s = args_ident { a @ s }

  /* ..., c) */
  | a = arg_ident ; Lparfin { a }

  /* ..., c) */
  | Lparfin { [] }

arg_ident:
  /* Argument */
  | a = Lvar { [ a ] }

block:
  /* { */
  | Lbracedeb ; b = block { b }

  /* instr; ... */
  | i = instr ; b = block { i @ b }

  /* } */
  | Lbracefin { [] }
;

instr:
  /* return x */
  | Lreturn ; e = expr ; Lsc { [ Return { expr = e ; pos = $startpos } ] }

  /* type v */
  | t = Ltype ; v = Lvar ; Lsc {
    [ Decl { name = v ; type_t = t ; pos = $startpos(t) } ]
  }

  /* type v = e */
  | t = Ltype ; v = Lvar ; Lassign ; e = expr ; Lsc
    { [ Decl { name = v ; type_t = t ; pos = $startpos(t) }
    ; Assign { var = v ; expr = e ; pos = $startpos(v) } ]
    }

  /* v = e */
  | v = Lvar ; Lassign ; e = expr ; Lsc {
    [ Assign { var = v ; expr = e ; pos = $startpos($2) } ]
  }

  /* function() */
  | f = Lvar ; a = args_expr ; Lsc {
    [ Do { expr = Call { func = f ; args = a ; pos = $startpos(a) } ; pos = $startpos} ]
  }

args_expr:
  /* ( */
  | Lpardeb ; s = args_expr { s }

  /* a, ... */
  | a = expr ; Lcomma ; s = args_expr { a :: s }

  /* ..., c) */
  | a = expr ; Lparfin { [ a ] }

  /* ..., c) */
  | Lparfin { [] }

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
