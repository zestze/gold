(* Ocamllex scanner for GOLD *)
(* Author: Aidan Rivera *)
(* Contributor: Ezekiel Reyna *)

{ open Parser }

rule token = parse 
	  [' ' '\t' '\r' '\n'] 	{ token lexbuf }
	| "//" 		{ comment lexbuf }
	| '"'		{ str (Buffer.create 16) lexbuf }
	| ';'		{ SEMI }
	| '{'		{ LBRACE }
	| '}'		{ RBRACE }
	| '('		{ LPAREN }
	| ')'		{ RPAREN }
	| '['		{ LBRACKET }
	| ']'		{ RBRACKET }
	| ','		{ COMMA }
	| '+'		{ PLUS }
	| '-'		{ MINUS }
	| '*'		{ TIMES }
	| '/'		{ DIVIDE }
	| '%'		{ MOD }
	| '='		{ ASSIGN }
	| "=="		{ EQ }
	| "!="		{ NEQ }
	| "<"		{ LT }
	| "<="		{ LEQ }
	| ">"		{ GT }
	| ">="		{ GEQ }
	| "&&"		{ AND }
	| "||"		{ OR }
	| "!"		{ NOT }
	| "func" 	{ FUNCTION }
	| "type"	{ TYPE }
	| "struct"	{ STRUCT }
	| "if"		{ IF }
	| "else"	{ ELSE }
	| "for"		{ FOR }
	| "int" 	{ INT }
	| "float"	{ FLOAT } 
	| "string"	{ STRING }
	| "bool"	{ BOOL }
	| "void"	{ VOID }
	| "true"	{ TRUE }
	| "false"	{ FALSE }
	| "return" 	{ RETURN }
	| "~"		{ DEREF }
	| "@"		{ REF }
	| ['0'-'9']+ as word { INTLIT(int_of_string word) }
	| ['0'-'9']+ ['.'] ['0'-'9']+ as word { FLOATLIT(float_of_string word) }
	| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* as word { ID(word) }
	| eof { EOF }
	| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }


and comment = parse
	  '\n' 	{ token lexbuf }
	| _ 	{ comment lexbuf }

and str strbuf = parse
	  '"' 		{ STRINGLIT( Buffer.contents strbuf) }
	(* escape characters *)
	| '\\' '"' 	{ Buffer.add_char strbuf '"'; str strbuf lexbuf }
	| '\\' '\\'	{ Buffer.add_char strbuf '\\'; str strbuf lexbuf } (*Should this be two \\s?*)
	| '\\' '/'  { Buffer.add_char strbuf '/'; str strbuf lexbuf }
	| '\\' 'n'  { Buffer.add_char strbuf '\n'; str strbuf lexbuf }
	| '\\' 't'  { Buffer.add_char strbuf '\t'; str strbuf lexbuf }
	| '\\' 'r'  { Buffer.add_char strbuf '\r'; str strbuf lexbuf } (* can add other escape chars later *)
	| [^ '\\' '"']+	{ Buffer.add_string strbuf (Lexing.lexeme lexbuf); str strbuf lexbuf }
	| eof 		{ raise (Failure("Unterminated String")) }
	| _ 		{ raise (Failure("Problem with string")) }
