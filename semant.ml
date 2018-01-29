(* semant.ml *)
(* Author: Aidan Rivera *)
(* Contributor: Ezekiel Reyna *)

open Ast

module StringMap = Map.Make(String)

(* Returns void if true, else throws an exception *)
let check program =

	(* HELPER FUNCTIONS *)
	let report_duplicate exceptf list =
		let rec helper = function
			  n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1 ))
			| _ :: t -> helper t
			| [] -> () in 
		helper (List.sort compare list) in
	let check_duplicate_definition exceptf n vars =
		if StringMap.mem n vars then raise (Failure (exceptf n)) else () in
	let check_not_void exceptf = function 
		  (Void, n) -> raise (Failure (exceptf n))
		| _ -> () in
	let check_member_not_void exceptf = function
		  (n, Void) -> raise (Failure (exceptf n))
		| _ -> () in
	let check_assign lvaluet rvaluet err = (*lvaluet in*)
		match lvaluet with
		| Array(lt, None) -> 
			(match rvaluet with
			| Array(rt, _) -> if lt == rt then lvaluet else raise err
			| _ -> raise err)
		| _ -> if lvaluet = rvaluet then lvaluet else raise err in


	(* CHECKING GLOBALS *)
	List.iter (check_not_void (fun n -> "illegal void global " ^ n)) program.globals;
	report_duplicate (fun n -> "duplicate global " ^ n) (List.map (fun fd -> fd.fname) program.functions);
	List.iter (fun t -> List.iter (check_member_not_void (fun n -> "illegal void tstruct member " ^ n)) t.members) program.tstructs;
	

	(* CHECKING FUNCTIONS *)
	let built_in_functions = [ "print" ; "println" ; "sprint" ; "input" ; "atoi" ] in 
	List.iter (fun name -> 
		if List.mem name (List.map (fun fd -> fd.fname) program.functions)
		then raise (Failure ("function " ^ name ^ " may not be defined")) else ();
		report_duplicate (fun n -> "duplicate function " ^ n) (List.map (fun fd -> fd.fname) program.functions)
	) built_in_functions;

	(* FUNCTION DECL FOR NAMED FUNCTIONS *)
	let built_in_decls = List.fold_left (fun m (n, t) -> StringMap.add n t m) StringMap.empty
		[("print", { typ=Void; fname="print"; var_args=false; formals = [(String, "x")]; body=[] });
		("println",{ typ=Void; fname="println"; var_args=false; formals = [(String, "x")]; body=[] });
		("sprint", { typ=String; fname="sprint"; var_args=true; formals = [(String, "x")]; body=[] }); 
		("input",  { typ=String; fname="input"; var_args=false; formals = []; body=[] }); 
		("atoi", { typ=Int; fname="atoi"; var_args=false; formals = [(String, "x")]; body=[] });
		("rand", { typ=Int; fname="rand"; var_args=false; formals = []; body=[] });
		("srand",{ typ=Void; fname="rand"; var_args=false; formals = []; body=[] })]
	in

	let function_decls = 
		List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls program.functions in 
	let function_decl s = try StringMap.find s function_decls
		with Not_found -> raise (Failure ("unrecognized function " ^ s)) in


	(* ENSURE "MAIN" IS DEFINED *)
	let _ = function_decl "main" in
	
	let check_function func = 
		List.iter (check_not_void (fun n -> 
			"illegal void formal " ^ n ^ " in " ^ func.fname)) func.formals;
		report_duplicate (fun n -> 
			"duplicate formal " ^ n ^ " in " ^ func.fname) (List.map snd func.formals);

		(* VARIABLE SYMBOL TABLE *)
		let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m) 
			StringMap.empty (program.globals @ func.formals) in
		let type_of_identifier vars s = 
			try StringMap.find s vars
			with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in 
	

		(* RETURN TYPE OF EXPRESSION OR THROW EXCEPTION *)	
		let rec expr locals = function 
 			| IntLit _ -> 		Int 
			| FloatLit _ ->		Float
			| StringLit _ -> 	String
			| BoolLit _ -> 		Bool
			| Noexpr -> 		Void
			| Variable s ->		type_of_identifier locals s
			| Binop(e1, op, e2) as e -> let t1 = expr locals e1 and t2 = expr locals e2 in
				(match op with
				| Add | Sub | Mult | Div | Mod when t1 = Int && t2 = Int -> Int
				| Add | Sub | Mult | Div | Mod when t1 = Float && t2 = Float -> Float 
				| Equal | Neq when t1 = t2 -> Bool
				| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
				| Less | Leq | Greater | Geq when t1 = Float && t2 = Float -> Bool
				| And | Or when t1 = Bool && t2 = Bool -> Bool
				| _ -> raise (Failure ("illegal binary operator " ^
					string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
					string_of_typ t2 ^ " in " ^ string_of_expr e)))
			| Unop(op, e) as ex -> let t = expr locals e in
				(match op with
				| Neg when t = Int -> Int
				| Neg when t = Float -> Float
				| Not when t = Bool -> Bool
				| _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
					string_of_typ t ^ " in " ^ string_of_expr ex)))
 			| Assign(var, e) as ex -> let lt = type_of_identifier locals var and rt = expr locals e in
				check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
						" = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
			| Call(fname, actuals) as call -> let fd = function_decl fname in
				if fd.var_args = false then
					(if List.length actuals != List.length fd.formals then 
						raise (Failure ("expecting " ^ string_of_int (List.length fd.formals)
							^ " arguments in " ^ string_of_expr call))
					else 
						List.iter2 (fun (ft, _) e -> let et = expr locals e in 
							ignore (check_assign ft et 
								(Failure ("illegal actual argument found " ^ 
								string_of_typ et ^ " expected " ^ 
								string_of_typ ft ^ " in " ^ string_of_expr e))))
							fd.formals actuals;
						fd.typ) 
				else 
	(*				List.iter2 (fun (ft, _) e -> let et = expr locals e in 
						ignore (check_assign ft et 
							(Failure ("illegal actual argument found " ^ 
							string_of_typ et ^ " expected " ^ 
							string_of_typ ft ^ " in " ^ string_of_expr e))))
	*)						
					fd.typ (*Change to check equality in all formals, not actuals *)
			| ArrayLit el ->
				let size = List.length el in
(* add more *)
				Array((expr locals (List.hd el)), Some size) 

			| Access (id, _) -> 
				(match type_of_identifier locals id with
				| Array(t, _) -> t
				| t -> t)
			| Ref s -> Pointer(type_of_identifier locals s)
			| Deref (s,e) ->
				(match e with
				| Some _ -> raise (Failure ("Access should be hit, not deref in " ^ string_of_expr (Deref(s, e))))
				| None -> 
					(match type_of_identifier locals s with
					| Pointer t -> t
					| _ -> raise(Failure ("Dereferencing non pointer value in " ^ string_of_expr (Deref(s,e))))))
			| RefChange (e1, e2) ->
				let expr_string = string_of_expr (RefChange(e1,e2)) in
				(match e1 with
				| Ref s -> Pointer(type_of_identifier locals s)
				| Deref (s,e) -> 
					(match e with
					| Some _ -> 
						(match type_of_identifier locals s with
						| Array(t, _) -> t
						| _ -> raise (Failure ("RefChange array error in " ^
							expr_string)))
					| None -> 
						(match type_of_identifier locals s with
						| Pointer t -> t
						| _ -> raise (Failure ("Pointer deref error in " ^ 
							expr_string))))
				| _ -> raise (Failure ("Changing unavailable ref " ^ expr_string)))
			in

		let check_bool_expr locals e = if expr locals e != Bool
			then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
			else () in

	
		(* VERIFY STATEMENT OR THROW EXCEPTION *)
		let rec stmt locals = function 
 			| Block sl -> 
				let rec check_block block_locals = function 
				| [Return _ as s] -> stmt block_locals s
				| Return _ :: _ -> raise (Failure "nothing may follow a return")
				| Block sl :: ss -> stmt block_locals (Block sl); check_block block_locals ss
				| s :: ss ->
					(match s with 
					| VDecl (t, id) -> 
						check_duplicate_definition (fun n -> "variable already defined " ^ n ^ " in " ^ func.fname) id block_locals; 
						let block_locals = StringMap.add id t block_locals in 
						check_block block_locals ss
					| VDeclAss (t,id,e) ->
						check_duplicate_definition (fun n -> "variable already defined " ^ n ^ " in " ^ func.fname) id block_locals;
						let block_locals = StringMap.add id t block_locals in
						ignore (expr block_locals e); check_block block_locals ss
					| _ -> stmt block_locals s; check_block block_locals ss)
				| [] -> () in 
				check_block locals sl
			| VDecl (_,_) -> () (* Placeholder, as this shouldn't be hit *)
			| VDeclAss(_,_,_) -> ()
			| Expr e -> ignore (expr locals e)
			| Return e -> 
				let t = expr locals e in 
				if t = func.typ then ()
				else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^ 
					string_of_typ func.typ ^ " in " ^ string_of_expr e))
			| If(p, b1, b2) -> check_bool_expr locals p; stmt locals b1; stmt locals b2
			| For(e1, e2, e3, st) -> ignore (expr locals e1); check_bool_expr locals e2;
				ignore (expr locals e3); stmt locals st
			| While(_, _) -> () in (* For pattern matching warning *)

		stmt symbols (Block func.body) (* Body of check function *) in 
	List.iter check_function program.functions (* Body of check *)
	

