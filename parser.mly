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
%token Ladd Lsub Lmul Ldiv Lrem Lseq Lsge Lsgt Lsle Lslt Lsne
%token Land Lor
%token Lif Lelse Lwhile

%left Ladd Lsub Lseq Lsge Lsgt Lsle Lslt Lsne
%left Lmul Ldiv Lrem
%left Land Lor

%type <Ast.Syntax.prog> prog

%start prog

%%

prog:
  /* Liste des définitions de fonction */
  | f = def ; b = prog { f @ b }
  /* Fin de programme */
  | Lend { [] }

def:
  /* Définition fonction : type fonction (...) block */
  | t = Ltype
  ; f = Lvar
  ; Lpardeb
  ; a = separated_list(Lcomma, arg)
  ; Lparfin
  ; b = block {
    [ Func { func = f ; type_t = t ; args = a ; code = b ; pos = $startpos(f) } ]
  }

arg:
  /* type v */
  | t = Ltype ; v = Lvar { Arg { type_t = t ; name = v } }

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
    [ Return { expr = Val { value = Void ; pos = $startpos($2) }
             ; pos = $startpos } ]
    }

  /* type v; */
  | t = Ltype ; v = Lvar ; Lsc {
    [ Decl { name = v ; type_t = t ; pos = $startpos } ]
  }

  /* type v = e; */
  | t = Ltype ; v = Lvar ; Lassign ; e = expr ; Lsc
    { [ Decl { name = v ; type_t = t ; pos = $startpos(v) }
    ; Assign { var = v ; expr = e ; pos = $startpos($3) } ]
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
    Val { value = Int (-n) ; pos = $startpos }
  }

  /* int */
  | n = Lint {
    Val { value = Int (n) ; pos = $startpos }
  }

  /* bool */
  | b = Lbool {
    Val { value = Bool (b) ; pos = $startpos }
  }

  /* string */
  | s = Lstr {
    Val { value = Str (s) ; pos = $startpos }
  }

  /* Variable */
  | v = Lvar {
    Var { name = v ; pos = $startpos }
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

  /* e % e */
  | a = expr ; Lrem ; b = expr {
    Call { func = "%rem" ; args = [ a ; b ] ; pos = $startpos($2) }
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

  /* e && e */
  | a = expr ; Land ; b = expr {
    Call { func = "%and" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* e || e */
  | a = expr ; Lor ; b = expr {
    Call { func = "%or" ; args = [ a ; b ] ; pos = $startpos($2) }
  }

  /* function(...) */
  | f = Lvar ; Lpardeb ; a = separated_list(Lcomma, expr) ; Lparfin {
    Call { func = f ; args = a ; pos = $startpos }
  }
;
