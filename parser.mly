%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lstr
%token <Ast.type_t> Ltype
%token <string> Lvar
%token Lend Lassign Lsc Lreturn
%token Lbracedeb Lbracefin
%token Lpardeb Lparfin Lcomma
%token Ladd Lsub Lmul Ldiv Lseq Lsge Lsgt Lsle Lslt Lsne
%token Lif Lelse Lwhile

%left Ladd Lsub Lmul Ldiv Lseq Lsge Lsgt Lsle Lslt Lsne

%left Lbracedeb Lparfin Lbracefin Lreturn
%left Ltype Lbool Lint Lvar Lstr
%left Lif Lwhile

%start prog

%type <Ast.Syntax.block> block
%type <Ast.Syntax.prog> prog
%type <Ast.Syntax.args> args_ident
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

  /* type a, ... */
  | t = Ltype ; a = arg_ident ; Lcomma ; s = args_ident { Arg { type_t = t
                                                              ; name = a
                                                              } :: s }

  /* type c) */
  | t = Ltype ; a = arg_ident ; Lparfin { [ Arg { type_t = t
                                                ; name = a } ] }

  /* ) */
  | Lparfin { [] }

arg_ident:
  /* Argument */
  | a = Lvar { a }

block:
  /* { */
  | Lbracedeb ; b = block { b }

  /* instr ... */
  | i = instr ; b = block { i @ b }

  /* } */
  | Lbracefin { [] }
;

instr:
  /* return x; */
  | Lreturn ; e = expr ; Lsc { [ Return { expr = e ; pos = $startpos } ] }

  /* return; */
  | Lreturn ; Lsc {
    [ Return { expr = Val { value = Void ; pos = $startpos }
             ; pos = $startpos } ]
    }

  /* type v; */
  | t = Ltype ; v = Lvar ; Lsc {
    [ Decl { name = v ; type_t = t ; pos = $startpos(t) } ]
  }

  /* type v = e; */
  | t = Ltype ; v = Lvar ; Lassign ; e = expr ; Lsc
    { [ Decl { name = v ; type_t = t ; pos = $startpos(t) }
    ; Assign { var = v ; expr = e ; pos = $startpos(v) } ]
    }

  /* v = e; */
  | v = Lvar ; Lassign ; e = expr ; Lsc {
    [ Assign { var = v ; expr = e ; pos = $startpos($2) } ]
  }

  /* e; */
  | e = expr ; Lsc {
    [ Do { expr = e ; pos = $startpos} ]
  }

  /* if (e) {} else {} */
  | Lif ; Lpardeb ; e = expr ; Lparfin ; b1 = block ; Lelse ; b2 = block {
    [ Cond { expr = e ; if_b = b1 ; else_b = b2 ; pos = $startpos } ]
  }

  /* if (e) {} */
  | Lif ; Lpardeb ; e = expr ; Lparfin ; b = block {
    [ Cond { expr = e ; if_b = b ; else_b = [] ; pos = $startpos } ]
  }

  /* while (e) {} */
  | Lwhile ; Lpardeb ; e = expr ; Lparfin ; b = block {
    [ Loop { expr = e ; block = b ; pos = $startpos } ]
  }

expr:
  /* -int */
  | Lsub ; n = Lint {
    Val { value = Int (-n) ; pos = $startpos(n) }
  }

  /* int */
  | n = Lint {
    Val { value = Int (n) ; pos = $startpos(n) }
  }

  /* bool */
  | b = Lbool {
    Val { value = Bool (b) ; pos = $startpos(b) }
  }

  /* string */
  | s = Lstr {
    Val { value = Str (s) ; pos = $startpos(s) }
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

  /* e == e */
  | a = expr ; Lseq ; b = expr {
    Call { func = "%seq" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e >= e */
  | a = expr ; Lsge ; b = expr {
    Call { func = "%sge" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e > e */
  | a = expr ; Lsgt ; b = expr {
    Call { func = "%sgt" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e <= e */
  | a = expr ; Lsle ; b = expr {
    Call { func = "%sle" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e < e */
  | a = expr ; Lslt ; b = expr {
    Call { func = "%slt" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e != e */
  | a = expr ; Lsne ; b = expr {
    Call { func = "%sne" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* function(a */
  | f = Lvar ; Lpardeb ; a = args_expr {
    Call { func = f ; args = a ; pos = $startpos(a) }
  }
;

args_expr:
  /* a, ... */
  | a = expr ; Lcomma ; s = args_expr { a :: s }

  /* c) */
  | a = expr ; Lparfin { [ a ] }

  /* ) */
  | Lparfin { [] }
