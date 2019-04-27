%{
(* START HEADER *)

open AST

let ghost = Region.ghost

(* END HEADER *)
%}

(* Entry points *)

%start program
%type <AST.t> program

%%

(* RULES

   This parser leverages Menhir-specific features, in particular
   parametric rules, rule inlining and primitives to get the source
   locations of tokens from the lexer engine generated by ocamllex.

     The Abstract Syntax Tree (AST) has been designed to be isomorphic
   to the grammar, that is, each grammatical rule (usually)
   corresponds to a node in the AST, except if it is inline. This
   enables simpler semantic actions, as we mostly have to pass the
   synthesised attributes in the productions to a data constructor of
   the AST.

     Auxiliary rules for handling source regions

     We define below two rules, [reg] and [oreg]. The former parses
   its argument and returns its synthesised value together with its
   region in the source code (that is, start and end positions --- see
   module [Region]). The latter discards the value and only returns
   the region: this is mostly useful for parsing keywords, because
   those can be easily deduced from the AST node and only their source
   region has to be recorded there.
*)

%inline reg(X):
  X { Region.make ~start:$symbolstartpos ~stop:$endpos, $1 }

%inline oreg(X):
  X { Region.make ~start:$symbolstartpos ~stop:$endpos }

(* Keywords, symbols, literals and virtual tokens *)

kwd(X) : oreg(X)    { $1 }
sym(X) : oreg(X)    { $1 }
ident  : reg(Ident) { $1 }
string : reg(Str)   { $1 }
eof    : oreg(EOF)  { $1 }

(* Compounded constructs *)

par(X): reg(sym(LPAR) X sym(RPAR) { $1,$2,$3 }) { $1 }

bracket(X): sym(LBRACK) X sym(RBRACK) { $1,$2,$3 }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those
   rules.
*)

(* Possibly empty sequence of items *)

seq(X):
  (**)     {     [] }
| X seq(X) { $1::$2 }

(* Non-empty sequence of items *)

nseq(X):
  X seq(X) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                        $1, [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empy separated sequence of items *)

sepseq(X,Sep):
  (**)           {    None }
| nsepseq(X,Sep) { Some $1 }

(* Non-empty comma-separated values *)

csv(X):
  reg(X sym(COMMA) nsepseq(X,sym(COMMA)) {let h,t = $3 in $1,($2,h)::t}) {$1}

(* Non-empty bar-separated productions *)

bsv(X):
  nsepseq(X,sym(VBAR)) { $1 }

(* Possibly empty semicolon-separated values between brackets *)

list__(X):
  reg(bracket(sepseq(X,sym(SEMI)))) { $1 }

(* Main *)

program:
  seq(statement) eof                                            { $1,$2 }

statement:
  reg(kwd(Let)          let_bindings     {$1,$2})           {    Let $1 }
| reg(kwd(Let) kwd(Rec) let_rec_bindings {$1,$2,$3})        { LetRec $1 }

(* Recursive definitions *)

let_rec_bindings:
  nsepseq(let_rec_binding, kwd(And))                               { $1 }

let_rec_binding:
  ident nseq(pattern) sym(EQ) expr           { $1, ghost, norm $2 $3 $4 }
| ident               sym(EQ) fun_expr(expr) { $1,    $2,            $3 }

(* Non-recursive definitions *)

let_bindings:
  nsepseq(let_binding, kwd(And))                                   { $1 }

let_binding:
  ident nseq(pattern) sym(EQ) expr     { let expr = Fun (norm $2 $3 $4)
                                         in Pvar $1, ghost, expr        }
| let_lhs             sym(EQ) expr     {                       $1,$2,$3 }

(* Patterns *)

let_lhs:
  reg(pattern sym(CONS) cons_pat {$1,$2,$3})                {  Pcons $1 }
| csv(pattern)                                              { Ptuple $1 }
| common_pattern                                            {        $1 }

common_pattern:
  ident                                                     {   Pvar $1 }
| sym(WILD)                                                 {  Pwild $1 }
| unit                                                      {  Punit $1 }
| reg(Int)                                                  {   Pint $1 }
| kwd(True)                                                 {  Ptrue $1 }
| kwd(False)                                                { Pfalse $1 }
| string                                                    {   Pstr $1 }
| list__(cons_pat)                                          {  Plist $1 }
| par(ptuple)                                               {   Ppar $1 }

ptuple:
  csv(cons_pat)                                             { Ptuple $1 }

unit:
  reg(sym(LPAR) sym(RPAR) {$1,$2})                                 { $1 }

cons_pat:
  reg(pattern sym(CONS) cons_pat {$1,$2,$3})                 { Pcons $1 }
| pattern                                                    {       $1 }

pattern:
  par(cons_pat)                                               { Ppar $1 }
| common_pattern                                              {      $1 }

(* Expressions *)

expr:
  common_expr(expr)                                        {         $1 }
| reg(conditional(expr))                                   {      If $1 }
| reg(match_expr(expr_no_match))                           {   Match $1 }

expr_no_match:
  common_expr(expr_no_match)                               {         $1 }
| reg(conditional(expr_no_match))                          {      If $1 }

common_expr(right_expr):
  reg(let_expr(right_expr))                                { LetExpr $1 }
| fun_expr(right_expr)                                     {     Fun $1 }
| csv(disj_expr)                                           {   Tuple $1 }
| disj_expr                                                {         $1 }

conditional(right_expr):
  if_then_else(right_expr)
| if_then(right_expr)                                              { $1 }

if_then(right_expr):
  kwd(If) expr kwd(Then) right_expr              { IfThen ($1,$2,$3,$4) }

if_then_else(right_expr):
  kwd(If) expr kwd(Then) closed_if kwd(Else) right_expr {
    IfThenElse ($1,$2,$3,$4,$5,$6) }

closed_if:
  common_expr(closed_if)                                    {       $1 }
| reg(if_then_else(closed_if))                              {    If $1 }
| reg(match_expr(closed_if_no_match))                       { Match $1 }

closed_if_no_match:
  common_expr(closed_if_no_match)                           {       $1 }
| reg(if_then_else(closed_if_no_match))                     {    If $1 }

match_expr(right_expr):
  kwd(Match) expr kwd(With) cases(right_expr) {
    $1,$2,$3, Utils.nsepseq_rev $4 }

cases(right_expr):
  case(right_expr)                                           { $1, [] }
| cases(expr_no_match) sym(VBAR) case(right_expr) {
    let h,t = $1 in $3, ($2,h)::t }

case(right_expr):
  let_lhs sym(ARROW) right_expr                            { $1,$2,$3 }

let_expr(right_expr):
  kwd(Let) let_bindings kwd(In) right_expr {
    LetIn ($1,$2,$3,$4)
  }
| kwd(Let) kwd(Rec) let_rec_bindings kwd(In) right_expr {
    LetRecIn ($1,$2,$3,$4,$5) }

fun_expr(right_expr):
  reg(kwd(Fun) nseq(pattern) sym(ARROW) right_expr {$1,$2,$3,$4}) {
    let reg, (kwd_fun, patterns, arrow, expr) = $1
    in norm ~reg:(reg, kwd_fun) patterns arrow expr }

disj_expr:
  reg(disj_expr sym(BOOL_OR) conj_expr {$1,$2,$3})         {    Or $1 }
| conj_expr                                                {       $1 }

conj_expr:
  reg(conj_expr sym(BOOL_AND) comp_expr {$1,$2,$3})        {   And $1 }
| comp_expr                                                {       $1 }

comp_expr:
  reg(comp_expr sym(LT) cat_expr {$1,$2,$3})               {    Lt $1 }
| reg(comp_expr sym(LE) cat_expr {$1,$2,$3})               {   LEq $1 }
| reg(comp_expr sym(GT) cat_expr {$1,$2,$3})               {    Gt $1 }
| reg(comp_expr sym(GE) cat_expr {$1,$2,$3})               {   GEq $1 }
| reg(comp_expr sym(EQ) cat_expr {$1,$2,$3})               {    Eq $1 }
| reg(comp_expr sym(NE) cat_expr {$1,$2,$3})               {   NEq $1 }
| cat_expr                                                 {       $1 }

cat_expr:
  reg(cons_expr sym(CAT) cat_expr {$1,$2,$3})              {   Cat $1 }
| cons_expr                                                {       $1 }

cons_expr:
  reg(add_expr sym(CONS) cons_expr {$1,$2,$3})             {  Cons $1 }
| add_expr                                                 {       $1 }

add_expr:
  reg(add_expr sym(PLUS)   mult_expr {$1,$2,$3})           {   Add $1 }
| reg(add_expr sym(MINUS)  mult_expr {$1,$2,$3})           {   Sub $1 }
| mult_expr                                                {       $1 }

mult_expr:
  reg(mult_expr sym(MULT) unary_expr {$1,$2,$3})           {  Mult $1 }
| reg(mult_expr sym(DIV)  unary_expr {$1,$2,$3})           {   Div $1 }
| reg(mult_expr kwd(Mod)  unary_expr {$1,$2,$3})           {   Mod $1 }
| unary_expr                                               {       $1 }

unary_expr:
  reg(sym(MINUS) core_expr {$1,$2})                       {   Neg $1 }
| reg(kwd(Not)   core_expr {$1,$2})                       {   Not $1 }
| primary_expr                                            {       $1 }

primary_expr:
  reg(primary_expr core_expr {$1,$2})                     {  Call $1 }
| core_expr                                               {       $1 }

core_expr:
  reg(Int)                                                {   Int $1 }
| ident                                                   {   Var $1 }
| string                                                  {   Str $1 }
| unit                                                    {  Unit $1 }
| kwd(False)                                              { False $1 }
| kwd(True)                                               {  True $1 }
| list__(expr)                                            {  List $1 }
| par(expr)                                               {   Par $1 }
