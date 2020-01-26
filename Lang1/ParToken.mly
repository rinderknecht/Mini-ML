%{
%}

(* Tokens (mirroring those defined in module Token) *)

%token MINUS "-"
%token PLUS  "+"
%token DIV   "/"
%token MULT  "*"

%token LPAR   "("
%token RPAR   ")"
%token LBRACK "["
%token RBRACK "]"

%token ARROW  "->"
%token VBAR   "|"
%token CONS   "::"
%token CAT    "^"

%token COMMA  ","
%token SEMI   ";"

%token WILD   "_"

%token EQ     "="
%token NE     "<>"
%token LT     "<"
%token GT     ">"
%token LE     "<="
%token GE     ">="

%token BOOL_OR   "||"
%token BOOL_AND  "&&"

%token <string> Ident "<ident>"
%token <string> Str   "<string>"
%token <Z.t>    Int   "<int>"

%token And    "and"
%token Else   "else"
%token False  "false"
%token Fun    "fun"
%token If     "if"
%token In     "in"
%token Let    "let"
%token Match  "match"
%token Mod    "mod"
%token Not    "not"
%token Rec    "rec"
%token Then   "then"
%token True   "true"
%token With   "with"

%token EOF

%%
