%{
(* START HEADER *)

open AST

(* END HEADER *)
%}


(* Entry points *)

%start program
%type <AST.t> program

%%

(* RULES *)

(* This parser leverages Menhir-specific features, in particular
   parametric rules, rule inlining and primitives to get the source
   locations of tokens from the lexer engine generated by ocamllex.

     We define below two rules, [reg] and [oreg]. The former parses
   its argument and returns its synthesised value together with its
   region in the source code (that is, start and end positions --- see
   module [Region]). The latter discards the value and only returns
   the region: this is mostly useful for parsing keywords, because
   those can be easily deduced from the AST node and only their source
   region has to be recorded there.
*)

%inline reg(X):
  X { let start  = Pos.from_byte $symbolstartpos
      and stop   = Pos.from_byte $endpos in
      let region = Region.make ~start ~stop
      in Region.{region; value=$1} }

%inline oreg(X):
  reg(X) { $1.Region.region }

(* Keywords, symbols, literals and virtual tokens *)

kwd(X) : oreg(X)     { $1 }
sym(X) : oreg(X)     { $1 }
ident  : reg(Ident)  { $1 }
constr : reg(Constr) { $1 }
string : reg(Str)    { $1 }
eof    : oreg(EOF)   { $1 }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Compound constructs *)

par(X): sym(LPAR) X sym(RPAR) { {lpar=$1; inside=$2; rpar=$3} }

brackets(X): sym(LBRACK) X sym(RBRACK) { {lbra=$1; inside=$2; rbra=$3} }

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

(* Inlined *)

%inline type_name  : ident { $1 }
%inline field_name : ident { $1 }

(* Non-empty comma-separated values (at least two values) *)

csv(X):
  X sym(COMMA) nsepseq(X,sym(COMMA)) { let h,t = $3 in $1,($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list_of(X):
  reg(brackets(sepseq(X,sym(SEMI)))) { $1 }

(* Main *)

program:
  seq(statement) eof                                          { $1,$2 }

statement:
  reg(kwd(Let)          let_bindings     {$1,$2})       {      Let $1 }
| reg(kwd(Let) kwd(Rec) let_rec_bindings {$1,$2,$3})    {   LetRec $1 }
| reg(type_decl)                                        { TypeDecl $1 }

(* Type declarations *)

type_decl:
  kwd(Type) type_name sym(EQ) type_expr                 { $1,$2,$3,$4 }

type_expr:
  reg(cartesian)                                         {   TProd $1 }
| reg(sum_type)                                          {    TSum $1 }
| reg(record_type)                                       { TRecord $1 }

cartesian:
  nsepseq(fun_type,sym(TIMES))                                   { $1 }

fun_type:
  core_type                                                 {      $1 }
| reg(arrow_type)                                           { TFun $1 }

arrow_type:
  core_type sym(ARROW) fun_type     { {domain=$1; arrow=$2; range=$3} }

core_type:
  type_name {
    TAlias $1
  }
| reg(core_type type_constr {$1,$2}) {
    let arg, constr = $1.value in
    let lpar, rpar = Region.ghost, Region.ghost in
    let arg = {lpar; inside=arg,[]; rpar} in
    TApp Region.{$1 with value = constr, arg}
  }
| reg(type_tuple type_constr {$1,$2}) {
    let arg, constr = $1.value in
    TApp Region.{$1 with value = constr, arg}
  }
| reg(par(reg(cartesian))) {
    let Region.{region; value={lpar; inside=prod; rpar}} = $1 in
    TPar Region.{region; value={lpar; inside = TProd prod; rpar}} }

type_constr:
  type_name { $1 }
| kwd(Set)  { Region.{value="set";  region=$1} }
| kwd(Map)  { Region.{value="map";  region=$1} }
| kwd(List) { Region.{value="list"; region=$1} }

type_tuple:
  par(csv(type_expr)) { $1 }

sum_type:
  nsepseq(reg(variant),sym(VBAR)) { $1 }

variant:
  constr kwd(Of) cartesian { {constr=$1; kwd_of=$2; product=$3} }

record_type:
  sym(LBRACE) sep_or_term_list(reg(field_decl),sym(SEMI))
  sym(RBRACE) {
    let elements, terminator = $2 in
    {opening = $1;
     elements = Some elements;
     terminator;
     closing = $3} }

field_decl:
  field_name sym(COLON) type_expr {
    {field_name=$1; colon=$2; field_type=$3} }

(* Recursive definitions *)

let_rec_bindings:
  nsepseq(let_rec_binding, kwd(And)) { $1 }

let_rec_binding:
  ident nseq(pattern) sym(EQ) expr {
    {pattern = $1; eq = Region.ghost;
     let_rec_rhs = Fun (norm $2 $3 $4)}
  }
| ident sym(EQ) fun_expr(expr) {
    {pattern = $1; eq = $2; let_rec_rhs = $3} }

(* Non-recursive definitions *)

let_bindings:
  nsepseq(let_binding, kwd(And)) { $1 }

let_binding:
  ident nseq(pattern) sym(EQ) expr {
    let let_rhs = Fun (norm $2 $3 $4) in
    {pattern = Pvar $1; eq = Region.ghost; let_rhs}
  }
| let_lhs sym(EQ) expr {
    {pattern=$1; eq=$2; let_rhs=$3} }

(* Patterns *)

let_lhs:
  reg(pattern sym(CONS) cons_pat {$1,$2,$3})              {  Pcons $1 }
| reg(csv(pattern))                                       { Ptuple $1 }
| base_pattern                                            {        $1 }

base_pattern:
  ident                                                   {   Pvar $1 }
| sym(WILD)                                               {  Pwild $1 }
| unit                                                    {  Punit $1 }
| reg(Int)                                                {   Pint $1 }
| kwd(True)                                               {  Ptrue $1 }
| kwd(False)                                              { Pfalse $1 }
| string                                                  {   Pstr $1 }
| list_of(cons_pat)                                       {  Plist $1 }
| reg(par(ptuple))                                        {   Ppar $1 }

ptuple:
  reg(csv(cons_pat))                                      { Ptuple $1 }

unit:
  reg(sym(LPAR) sym(RPAR) {$1,$2})                               { $1 }

cons_pat:
  reg(pattern sym(CONS) cons_pat {$1,$2,$3})               { Pcons $1 }
| pattern                                                  {       $1 }

pattern:
  reg(par(cons_pat))                                        { Ppar $1 }
| base_pattern                                              {      $1 }

(* Expressions *)

expr:
  base_cond__open(expr)                                    {       $1 }
| match_expr(base_cond)                                    { Match $1 }

base_cond__open(x):
  base_expr(x)
| conditional(x)                                                 { $1 }

base_cond:
  base_cond__open(base_cond)                                     { $1 }

base_expr(right_expr):
  let_expr(right_expr)
| fun_expr(right_expr)
| disj_expr                                              {         $1 }
| reg(csv(disj_expr))                                    {   Tuple $1 }
| reg(sequence)                                          {     Seq $1 }

conditional(right_expr):
  if_then_else(right_expr)
| if_then(right_expr)                                    {      If $1 }

if_then(right_expr):
  reg(kwd(If) expr kwd(Then) right_expr {$1,$2,$3,$4})   {  IfThen $1 }

if_then_else(right_expr):
  reg(kwd(If) expr kwd(Then) closed_if kwd(Else) right_expr {
    $1,$2,$3,$4,$5,$6 })                              { IfThenElse $1 }

base_if_then_else__open(x):
  base_expr(x)                                             {       $1 }
| if_then_else(x)                                          {    If $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else)               {       $1 }

closed_if:
  base_if_then_else__open(closed_if)                       {       $1 }
| match_expr(base_if_then_else)                            { Match $1 }

match_expr(right_expr):
  reg(kwd(Match) expr kwd(With) cases(right_expr) {
    $1,$2,$3, Utils.nsepseq_rev $4 })                            { $1 }

cases(right_expr):
  case(right_expr)                                           { $1, [] }
| cases(base_cond) sym(VBAR) case(right_expr) {
    let h,t = $1 in $3, ($2,h)::t }

case(right_expr):
  let_lhs sym(ARROW) right_expr                            { $1,$2,$3 }

let_expr(right_expr):
  reg(kwd(Let) let_bindings kwd(In) right_expr {$1,$2,$3,$4}) {
    LetIn $1
  }
| reg(kwd(Let) kwd(Rec) let_rec_bindings kwd(In) right_expr {
        $1,$2,$3,$4,$5 }) {
    LetRecIn $1 }

fun_expr(right_expr):
  reg(kwd(Fun) nseq(pattern) sym(ARROW) right_expr {$1,$2,$3,$4}) {
    let Region.{region; value = kwd_fun, patterns, arrow, expr} = $1
    in Fun (norm ~reg:(region, kwd_fun) patterns arrow expr) }

sequence:
  kwd(Begin) sepseq(expr,sym(SEMI)) kwd(End) {
    {kwd_begin = $1; sequence = $2; kwd_end = $3} }

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
  reg(add_expr sym(PLUS)  mult_expr {$1,$2,$3})            {   Add $1 }
| reg(add_expr sym(MINUS) mult_expr {$1,$2,$3})            {   Sub $1 }
| mult_expr                                                {       $1 }

mult_expr:
  reg(mult_expr sym(TIMES) unary_expr {$1,$2,$3})          {  Mult $1 }
| reg(mult_expr sym(SLASH) unary_expr {$1,$2,$3})          {   Div $1 }
| reg(mult_expr kwd(Mod)   unary_expr {$1,$2,$3})          {   Mod $1 }
| unary_expr                                               {       $1 }

unary_expr:
  reg(sym(MINUS) core_expr {$1,$2})                        {   Neg $1 }
| reg(kwd(Not)   core_expr {$1,$2})                        {   Not $1 }
| call_expr                                                {       $1 }

call_expr:
  reg(call_expr core_expr {$1,$2})                         {  Call $1 }
| core_expr                                                {       $1 }

core_expr:
  reg(Int)                                                 {    Int $1 }
| ident                                                    {    Var $1 }
| string                                                   {    Str $1 }
| unit                                                     {   Unit $1 }
| kwd(False)                                               {  False $1 }
| kwd(True)                                                {   True $1 }
| list_of(expr)                                            {   List $1 }
| reg(par(expr))                                           {    Par $1 }
| constr                                                   { Constr $1 }
