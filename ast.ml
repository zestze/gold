(* ast.ml *)
(* Author: Aidan Rivera *)
(* Contributor: Ezekiel Reyna *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | 
	      Geq | And | Or

type uop = Neg | Not 

type typ =
	| Int | String | Bool | Void | Float
	| Array of typ * int option
	| Pointer of typ

type bind = typ * string

type member = string * typ

type expr =
	| IntLit of int		| Noexpr
	| StringLit of string 	| Call of string * expr list
	| BoolLit of bool	| Assign of string * expr
	| Variable of string	| Binop of expr * op * expr
	| Unop of uop * expr    | FloatLit of float
	| ArrayLit of expr list	| Access of string * expr
	| RefChange of expr * expr
	| Deref of string * expr option 	
	| Ref of string

type stmt =
	| VDeclAss of typ * string * expr
	| VDecl of typ * string
	| Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
 
type func_decl = {
	typ :		typ;
	fname :		string;
	var_args : 	bool;	
	formals :	bind list;
	body :		stmt list;
} 

type tstruct = {
	tname :		string;
	members :	member list; 

}

type program = {
	globals :	bind list;
	functions :	func_decl list;
	tstructs :	tstruct list;(* Change later *)
}

let string_of_op = function
	| Add ->	    "+"
	| Sub ->	    "-"
	| Mult ->	    "*"
	| Div ->	    "/"
	| Mod -> 	    "%"
	| Equal -> 	    "=="
	| Neq -> 	    "!="
	| Less -> 	    "<"
	| Leq -> 	    "<="
	| Greater -> 	">"
	| Geq -> 	    ">="
	| And -> 	    "&&"
	| Or -> 	    "||"

let string_of_uop = function
	| Neg -> "-"
	| Not -> "!"

let rec string_of_typ = function
	| Int -> 	"int"
	| String -> 	"string"
	| Bool ->	"bool"
	| Void -> 	"void"
	| Float ->	"float"
	| Pointer t ->	string_of_typ t ^ " ~" 
	| Array (t, n) -> match n with
		| Some i -> string_of_typ t ^ "[" ^ string_of_int i ^  "]"
		| None -> string_of_typ t ^ "[]"

let rec string_of_expr = function
	| IntLit i ->		string_of_int i
	| FloatLit f ->		string_of_float f
	| StringLit s ->	"\"" ^ s ^ "\""
	| BoolLit(true) ->	"true"
	| BoolLit(false) ->	"false"
	| Variable s ->		s
	| Noexpr ->		"void"
	| Binop (e1, o, e2) ->	string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
	| Unop(o, e) -> 	string_of_uop o ^ string_of_expr e
 	| Assign (s, e) ->	s ^ " = " ^ string_of_expr e
	| Call (s, el) ->	s ^ "(" ^ (List.fold_left (fun b a -> b ^ " " ^ string_of_expr a ^ " ") "" el) ^ ")"
	| ArrayLit el -> 	"{" ^ (List.fold_left (fun b a -> b ^ " " ^ string_of_expr a ^ ", ") "" el) ^ "}"
	| Access (s, e) -> 	s ^ "[" ^ string_of_expr e ^ "]"
	| RefChange (e1, e2) ->	string_of_expr e1 ^ " = " ^ string_of_expr e2
	| Ref s ->		"@" ^ s
	| Deref (s, e) -> match e with
		| Some e' ->	s ^ "[" ^ string_of_expr e' ^ "]"
		| None ->	"~" ^ s

let rec string_of_stmt = function
	| VDecl(t, id) -> 	string_of_typ t ^ " " ^ id ^ ";\n"
	| VDeclAss(t, id, e) ->	string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ ";\n"
	| Block(stmts) -> 	"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> 	string_of_expr expr ^ ";\n"
	| Return(expr) -> 	"return " ^ string_of_expr expr ^ ";\n"
	| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
	| If(e, s1, s2) ->  	"if (" ^ string_of_expr e ^ ")\n" ^
		string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
	| For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
		string_of_expr e3  ^ ") " ^ string_of_stmt s
	| While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  
let string_of_fdecl fdecl =
	string_of_typ fdecl.typ ^ " " ^ fdecl.fname ^ "(" ^ 
	String.concat ", " (List.map snd fdecl.formals) ^ ")\n{\n" ^
	String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_program program =
	String.concat "" (List.map (fun (t,id) -> string_of_stmt (VDecl (t,id))) program.globals) ^ "\n" ^ 
	String.concat "\n" (List.map string_of_fdecl program.functions) (* Add tstructs later *)
