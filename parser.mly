/* 
parser.mly
Author: Aidan Rivera
Contributor: Ezekiel Reyna
*/

%{
	open Ast
%}


%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR FUNCTION
%token TYPE STRUCT INT STRING BOOL VOID FLOAT
%token DEREF REF
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRINGLIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right DEREF REF
%right NOT NEG

%start program
%type <Ast.program> program

%%
program:
	decls EOF { $1 }

decls: 
	  /* Base */	{ { globals=[]; functions=[]; tstructs=[] } }
	| decls vdecl	{ { globals=($2 :: $1.globals); functions=$1.functions; tstructs=$1.tstructs } }
	| decls fdecl 	{ { globals=$1.globals; functions=($2 :: $1.functions); tstructs=$1.tstructs } }
	| decls tdecl	{ { globals=$1.globals; functions=$1.functions; tstructs=($2 :: $1.tstructs) } }

fdecl:
	FUNCTION ID LPAREN formals_opt RPAREN typ LBRACE stmt_list RBRACE { 
		{ typ = $6;
		  fname = $2;
		  var_args = false;
		  formals = $4;
		  body = List.rev $8
		} 
	}

tdecl: /* maybe change to typ inst. of TYPE */
	TYPE ID STRUCT LBRACE members RBRACE { 
		{ tname = $2;
		  members = $5
		}
	}

members:
	  { [] }
	| mem_list { List.rev $1 }

mem_list:
	  /*Base*/	{ [] }
	| mem_list ID typ SEMI { ($2, $3) :: $1 }

formals_opt:
	  { [] }
	| formal_list { List.rev $1 } 

formal_list:
	  typ ID 	{ [($1, $2)] } 
	| formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
    	| FLOAT		{ Float }
	| typ LBRACKET INTLIT RBRACKET 	{ Array($1, Some $3) }
	| typ LBRACKET RBRACKET		{ Array($1, None) }
	| INT		{ Int }
	| STRING	{ String }
	| BOOL		{ Bool }
	| VOID 		{ Void }
	| typ DEREF 	{ Pointer($1) }

vdecl:
	typ ID SEMI { ($1, $2) }

vdecl_stmt:
	| typ ID SEMI { VDecl($1, $2) }
	| typ ID ASSIGN expr SEMI { VDeclAss($1, $2, $4) }
	| typ ID ASSIGN array_lit SEMI { VDeclAss($1, $2, $4) }

stmt_list:
	  /*Base*/	{ [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI			{ Expr $1 }
	| vdecl_stmt			{ $1 }
	| RETURN SEMI			{ Return Noexpr }
	| RETURN expr SEMI		{ Return $2 }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
		{ For($3, $5, $7, $9) }
 	| LBRACE stmt_list RBRACE	{ Block(List.rev $2) }

expr_opt:
	  /* nothing */ { Noexpr }
	| expr          { $1 }

expr:
	  INTLIT 			{ IntLit($1) }
	| FLOATLIT			{ FloatLit($1) }
	| STRINGLIT 			{ StringLit($1) }
	| TRUE				{ BoolLit(true) }
	| FALSE				{ BoolLit(false) }
	| ID				{ Variable($1) }
   	| expr PLUS	expr 		{ Binop($1, Add,	$3) }
	| expr MINUS	expr 		{ Binop($1, Sub,	$3) }
	| expr TIMES	expr 		{ Binop($1, Mult,	$3) }
	| expr DIVIDE	expr 		{ Binop($1, Div,	$3) }
	| expr MOD 	expr		{ Binop($1, Mod,	$3) }
	| expr EQ	expr 		{ Binop($1, Equal,	$3) }
	| expr NEQ	expr 		{ Binop($1, Neq,	$3) }
	| expr LT	expr 		{ Binop($1, Less,	$3) }
	| expr LEQ	expr 		{ Binop($1, Leq,	$3) }
	| expr GT	expr 		{ Binop($1, Greater,	$3) }
	| expr GEQ	expr 		{ Binop($1, Geq,	$3) }
	| expr AND	expr 		{ Binop($1, And,	$3) }
	| expr OR	expr 		{ Binop($1, Or,		$3) } 
	| MINUS expr %prec NEG		{ Unop(Neg, $2) }
	| NOT expr			{ Unop(Not, $2) }
 	| ID ASSIGN	expr   		{ Assign($1, $3) }
	| ID ASSIGN 	array_lit	{ Assign($1, $3) }
	| ID LPAREN actuals_opt RPAREN	{ Call($1, $3) }
	| LPAREN expr RPAREN		{ $2 }
	| ID LBRACKET expr RBRACKET	{ Access($1, $3) }
	| lvalue ASSIGN expr   		{ RefChange($1, $3) }
	| DEREF ID			{ Deref($2, None) }
	| REF ID 			{ Ref($2)   }

lvalue:
	| ID LBRACKET expr RBRACKET	{ Deref($1, Some $3) }
	| DEREF ID			{ Deref($2, None)  }
	| REF ID 			{ Ref($2)   }


array_lit:
	| LBRACE actuals_opt RBRACE	{ ArrayLit($2) }

actuals_opt:
		{ [] }
	| actuals_list	{ List.rev $1 }

actuals_list:
	  expr	{ [$1] }
	| actuals_list COMMA expr { $3 :: $1 }


