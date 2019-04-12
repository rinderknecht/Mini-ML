%{
%}

(* Tokens (mirroring those defined in module Token) *)

%token MINUS
%token PLUS
%token DIV
%token MULT

%token LPAR
%token RPAR
%token LBRACK
%token RBRACK

%token ARROW
%token BAR
%token CONS
%token CAT

%token COMMA
%token SEMI

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
%token <string> Str

%token <Z.t> Int

%token And
%token Else
%token False
%token Fun
%token If
%token In
%token Let
%token Match
%token Mod
%token Not
%token Rec
%token Then
%token True
%token With

%token EOF

%%
