%{
(* START HEADER *)
open Utils
open AST

let comp_pat pat expr =
  let apply p a =
    match p with
      Pvar x   -> Fun (x,a)
    | Pwild    -> Fun (gen_sym (), a)
    | Ptuple _ -> let fresh = gen_sym () in
                  Fun (fresh, LetIn (((p, mk_var fresh),[]), a))
  in List.fold_right apply pat expr

let squeeze_par: expr -> core_expr = function
  BoolExpr
    (ConjExpr
       (CompExpr
          (AddExpr
             (MultExpr
                (UnaryExpr
                   (PrimaryExpr
                      (CoreExpr (Par _ as p)))))))) -> p
| e -> Par e

(* END HEADER *)
%}

(* Entry points *)

%start program
%type <AST.t> program

%%

(* RULES *)

(* Auxiliary rules *)

disj(X): d=X { d }

opt(X): { None } | v=X { Some v }
%inline iopt(X): { None } | v=X { Some v }

nseq(X): h=X t=seq(X) { h, t }

%inline seq(X):
            { [] }
| l=nseq(X) { let h,t = l in h::t }

sepseq(X,Sep): opt(nsepseq(X,Sep)) { $1 }

nsepseq(X,Sep):
  X                      { $1,[] }
| X Sep l=nsepseq(X,Sep) { let h,t = l in $1,h::t }

%inline par(X):
  LPAR X RPAR { $2 }

%inline
csv(X): X COMMA nsepseq(X,COMMA) { $1, $3 }

(* Main *)

program:
  nseq(top_expr) EOF { fst $1 }

(*item:
  definition {}
| expr       { $1 }
 *)
top_expr:
  Let let_binds In expr         { LetIn ($2,$4)                 }
| Let Rec rec_binds In expr     { LetRecIn ($3,$5)              }

expr:
  top_expr                      { $1 }
| If expr Then expr Else expr   { If ($2,$4,$6)                 }
| Fun nseq(pattern) ARROW expr  { comp_pat (nseq_to_list $2) $4 }
| bool_expr                     { BoolExpr $1                   }
| bool_expr COMMA nsepseq(bool_expr,COMMA) {
                                  Tuple ($1 :: nseq_to_list $3) }

let_binds:
  nsepseq(let_bind,And) { $1 }

let_bind:
  pattern seq(pattern) EQ expr {
    $1, comp_pat $2 $4 }
| pattern COMMA nsepseq(pattern,COMMA) EQ expr {
    Ptuple ($1 :: nseq_to_list $3), $5 }

rec_binds:
  nsepseq(rec_bind,And) { $1 }

rec_bind:
  WILD EQ expr               { RWild, $3               }
| Ident seq(pattern) EQ expr { RVar $1, comp_pat $2 $4 }

pattern:
  Ident             { Pvar $1                                  }
| WILD              { Pwild                                    }
| par(pattern)      { $1                                       }
| par(csv(pattern)) { Ptuple (fst $1 :: nseq_to_list (snd $1)) }

bool_expr:
  bool_expr BOOL_OR conj_expr { Or ($1,$3)  }
| conj_expr                   { ConjExpr $1 }

conj_expr:
  conj_expr BOOL_AND comp_expr { And ($1,$3) }
| comp_expr                    { CompExpr $1 }

comp_expr:
  comp_expr LT add_expr { Lt ($1,$3) }
| comp_expr LE add_expr { Le ($1,$3) }
| comp_expr GT add_expr { Gt ($1,$3) }
| comp_expr GE add_expr { Ge ($1,$3) }
| comp_expr EQ add_expr { Eq ($1,$3) }
| comp_expr NE add_expr { Ne ($1,$3) }
| add_expr              { AddExpr $1 }

add_expr:
  add_expr PLUS  mult_expr { Add ($1,$3) }
| add_expr MINUS mult_expr { Sub ($1,$3) }
| mult_expr                { MultExpr $1 }

mult_expr:
  mult_expr MULT unary_expr { Mult ($1,$3) }
| mult_expr DIV  unary_expr { Div  ($1,$3) }
| unary_expr                { UnaryExpr $1 }

unary_expr:
  MINUS primary_expr { Minus $2       }
| primary_expr       { PrimaryExpr $1 }

primary_expr:
  nseq(core_expr) {
    match $1 with
      f, arg::args -> Apply (f,(arg,args))
    | e,        [] -> CoreExpr e
  }
| Not primary_expr { Not $2 }

core_expr:
  Ident     { Var $1                 }
| Nat       { Nat (int_of_string $1) }
| Str       { Str $1                 }
| True      { True                   }
| False     { False                  }
| par(expr) { squeeze_par $1         }
