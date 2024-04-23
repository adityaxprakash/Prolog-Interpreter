%{
  open Helper
%}  

%token <string> LOWERCASE_IDENT UPPERCASE_IDENT
%token <int> NUMBER
%token DOT 
%token IMPLIES 
%token COMMA 
%token LPAREN RPAREN LSQBRACK RSQBRACK 
%token PIPE
%token QUERY 
%token TRUE NOT FAIL FALSE UNDERSCORE CUT
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ GT LT GTE LTE
%token IS 
%token EOF

%start program goal
%type <Helper.goal> goal
%type <Helper.program> program
%type <Helper.clause> clause

%left PLUS MINUS
%left TIMES DIVIDE
%left EQ NEQ GT LT GTE LTE
%left NOT
%nonassoc IS

%%
goal:
	| atomic_formula_list DOT { Goal $1 }

program:
	| EOF {[] }
	| clauses EOF { $1 }

clauses:
	| clause { [$1] }
	| clause clauses { $1 :: $2 }

clause:
	| atomic_formula DOT { (Head $1, Body []) }
	| atomic_formula IMPLIES atomic_formula_list DOT { (Head $1, Body $3) }

atomic_formula_list:
	| atomic_formula { [$1] }
	| atomic_formula COMMA atomic_formula_list { $1 :: $3 }

atomic_formula:
	| LOWERCASE_IDENT { Atom ($1, []) }
	| LOWERCASE_IDENT LPAREN terms_list RPAREN { Atom ($1, $3) }

	| TRUE { Helper.true_atom }
	| FAIL { Helper.fail_atom }
	| FALSE { Helper.false_atom }
	| CUT { Helper.cut_atom }
	| NOT atomic_formula { Not $2 }

	| term IS term { Atom("_is", [$1; $3])}
	| term EQ term { Atom("=", [$1; $3]) }
	| term NEQ term { Atom("\\=", [$1; $3]) }
	| term GT term { Atom(">", [$1; $3]) }
	| term LT term { Atom("<", [$1; $3]) }
	| term GTE term { Atom(">=", [$1; $3]) }
	| term LTE term { Atom("=<", [$1; $3]) }

terms_list:
	| term { [$1] }
	| term COMMA terms_list { $1 :: $3 }
	
term:
	| LPAREN term RPAREN { $2 }

	| LOWERCASE_IDENT { Func ($1, []) }
	| LOWERCASE_IDENT LPAREN terms_list RPAREN { Func ($1, $3) }
	| UPPERCASE_IDENT { Var $1 }
	| NUMBER { Num $1 }
	| UNDERSCORE { Wildcard }    

	| term PLUS term { Func("+",[$1;$3]) }
	| term MINUS term { Func("-",[$1;$3]) }
	| term TIMES term { Func("*",[$1;$3]) }
	| term DIVIDE term { Func("/",[$1;$3]) }
	| list { $1 }

list:
	| LSQBRACK RSQBRACK { Func("_empty_list", []) }
	| LSQBRACK list_body RSQBRACK { $2 }

list_body:
	| term { Func("_list", [$1 ; Func("_empty_list", [])])}
	| term COMMA list_body { Func("_list",[$1; $3]) }
	| term PIPE term { Func("_list", [$1; $3]) }

%%
