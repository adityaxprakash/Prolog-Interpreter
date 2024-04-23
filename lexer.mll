{
open Parser 
exception Failed of string
}

rule read = parse
	| [' ' '\t' '\n']+ { read lexbuf } 
	| eof { EOF }
	| '.' { DOT }
	| ":-" { IMPLIES }
	| ',' { COMMA }
	| '_' { UNDERSCORE }

	| "fail" { FAIL }
	| "true" { TRUE }
    | "false" { FALSE }
	| '!' {CUT}
	| "is" { IS }

    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LSQBRACK }
    | ']' {RSQBRACK}
    | '|' { PIPE }

    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { TIMES }
    | '/' { DIVIDE }

    | "=" { EQ }
    | "\\=" {NEQ}
    | "<" { LT }
    | ">" { GT } 
    | ">=" { GTE }
    | "=<" { LTE }
    | "\\+" { NOT }
	| "not" { NOT }

    | ['A'-'Z'][ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]* as lxm { UPPERCASE_IDENT lxm }
    | ['a'-'z'][ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]* as lxm { LOWERCASE_IDENT lxm }
    | ['0'-'9']+ as lxm { NUMBER(int_of_string lxm) }
    | '%' [^ '\n']* { read lexbuf}
    | "/*" { multiline_comment 1 lexbuf }
    | "*/" {raise (Failed "Unmatched closing comment")}
    | _ as ch { raise (Failed ("Invalid Character entered:" ^ (Char.escaped ch))) }

and multiline_comment count = parse
	| "/*" { multiline_comment (count+1) lexbuf }
	| "*/" { if count = 1 then read lexbuf else multiline_comment (count-1) lexbuf }
	| eof { raise (Failed "Opening comment never closed") }
	| _ { multiline_comment count lexbuf }