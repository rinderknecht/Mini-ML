%{
%}

(* Tokens (mirroring those defined in module Token) *)

%token MINUS
%token PLUS
%token DIV
%token MULT

%token LPAR
%token RPAR

%token ARROW
%token COMMA
%token WILD

%token EQ
%token NE
%token LT
%token GT
%token LE
%token GE

%token BOOL_OR
%token BOOL_AND

%token <string> Ident
%token <string> Nat
%token <string> Str

%token Let
%token Rec
%token And
%token In
%token Fun
%token If
%token Then
%token Else
%token True
%token False
%token Not

%token EOF

%%
