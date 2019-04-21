%{
%}

(* Tokens (mirroring those defined in module Token) *)

%token MINUS
%token PLUS
%token SLASH
%token TIMES

%token LPAR
%token RPAR
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE

%token ARROW
%token CONS
%token CAT

%token COMMA
%token SEMI
%token COLON
%token VBAR

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
%token <string> Constr
%token <string> Str

%token <Z.t> Int

%token And
%token Begin
%token Else
%token End
%token False
%token Fun
%token If
%token In
%token Let
%token List
%token Map
%token Match
%token Mod
%token Not
%token Of
%token Rec
%token Set
%token Then
%token True
%token Type
%token With

%token EOF

%%
