(* codegen.ml *)
(* Author: Aidan Rivera *)
(* Contributor: Ezekiel Reyna *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

exception Foo of string

let translate program =
	let context = L.global_context() in 
	let the_module = L.create_module context "Gold"
	
	and i32_t 	= L.i32_type	context		(* int *)
	and flt_t	= L.float_type context  	(* 32 bit float *)
	and i8_t	= L.i8_type	context		(* for print *)
	and i1_t	= L.i1_type	context		(* bool *)
	and str_t 	= L.pointer_type (L.i8_type context)
	and void_t	= L.void_type	context  	(* void *)
	and array_t	= L.array_type
	and pointer_t	= L.pointer_type in

	let rec ltype_of_typ = function (* Llvm type for Ast type *)
		  A.Int -> 	i32_t
		| A.Float -> flt_t
		| A.String -> 	str_t
		| A.Bool ->	i1_t
		| A.Void -> 	void_t 
		| A.Pointer t -> pointer_t (ltype_of_typ t)
		| A.Array(t,n) -> match n with
			| Some i -> array_t (ltype_of_typ t) i
			| None -> pointer_t (ltype_of_typ t) 	in

	(* Initialize global variables *)
	let global_vars =
		let global_var m (t, n) =
			let init = L.const_int (ltype_of_typ t) 0
			in StringMap.add n (L.define_global n init the_module) m in 
		List.fold_left global_var StringMap.empty program.A.globals in

	(* Declare external function print *)
	let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
	let print_func = (* For print and println *)
		L.declare_function "printf" printf_t the_module in(*If this isnt prinf hello.gold doesnt compile*)
	let sprintf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
	let sprint_func =
		L.declare_function "sprintf" sprintf_t the_module in
	let input_t = L.function_type (pointer_t i8_t) [| L.pointer_type i8_t |] in
	let input_func = 					(* Uses gets, I know it's dangerous *)
		L.declare_function "gets" input_t the_module in 
	let atoi_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
	let atoi_func =
		L.declare_function "atoi" atoi_t the_module in
	let rand_t = L.var_arg_function_type i32_t [| |] in
	let rand_func =
		L.declare_function "rand" rand_t the_module in
	let time_t = L.var_arg_function_type i32_t [| L.pointer_type i32_t |] in
	let time_func = 
		L.declare_function "time" time_t the_module in
	let srand_t = L.var_arg_function_type i32_t [| i32_t |] in
	let srand_func =
		L.declare_function "srand" srand_t the_module in

	(* Safe stdin support *)
(*	let file_t = L.named_struct_type context "file" in
	let file_ptr_t = pointer_t file_t in
	let fdopen_t = L.var_arg_function_type i32_t [| i32_t ; pointer_t i8_t |] in
	let fdopen_func = L.declare_function "fdopen" fdopen_t the_module in
	let mode_ptr = L.define_global "r" (L.const_array i8_t [| L.const_int i8_t 0 ; L.const_int i8_t 0 |]) the_module in
*)


	(* Define each function *)
	let function_decls = 
		let function_decl m fdecl =
			let name = fdecl.A.fname
			and formal_types = Array.of_list
				(List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.formals)
			in let ftype = 
				L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
			StringMap.add name (L.define_function name ftype the_module, fdecl) m in 
		List.fold_left function_decl StringMap.empty program.A.functions in

	(* Fill in function body *)
	let build_function_body fdecl = 
		let (the_function, _) =
			StringMap.find fdecl.A.fname function_decls in
		let builder =
			L.builder_at_end context (L.entry_block the_function) in 
	
(*		if fdecl.A.fname = "main" then 
			ignore(L.build_call fdopen_func [| L.const_int i32_t 0; 
				L.const_in_bounds_gep mode_ptr [| mode_ptr ; L.const_int i32_t 0; L.const_int i32_t 0|] |] "stdin" builder);
*)

		let local_vars =
			let add_formal m (t, n) p = L.set_value_name n p;
				let local = L.build_alloca (ltype_of_typ t) n builder in
				ignore (L.build_store p local builder);
				StringMap.add n local m in
			List.fold_left2 add_formal StringMap.empty fdecl.A.formals 
				(Array.to_list (L.params the_function)) in

		let lookup n vars = try StringMap.find n vars 
			with Not_found -> StringMap.find n global_vars in

		let rec expr builder local_vars = function 
			  A.IntLit i -> L.const_int i32_t i
			| A.FloatLit f -> L.const_float flt_t f
			| A.StringLit s -> L.build_global_stringptr s "str" builder
			| A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
 			| A.Noexpr -> L.const_int i32_t 0
			| A.Variable s -> L.build_load (lookup s local_vars) s builder
			| A.Binop (e1, op, e2) ->
				let e1' = expr builder local_vars e1
				and e2' = expr builder local_vars e2 in
				let int_bopr opr =
					(match opr with
					  A.Add		-> L.build_add
					| A.Sub		-> L.build_sub
					| A.Mult	-> L.build_mul
					| A.Div		-> L.build_sdiv
					| A.Mod		-> L.build_srem
					| A.And		-> L.build_and
 					| A.Or		-> L.build_or
					| A.Equal	-> L.build_icmp L.Icmp.Eq
					| A.Neq		-> L.build_icmp L.Icmp.Ne
					| A.Less	-> L.build_icmp L.Icmp.Slt
					| A.Leq		-> L.build_icmp L.Icmp.Sle
					| A.Greater	-> L.build_icmp L.Icmp.Sgt
					| A.Geq		-> L.build_icmp L.Icmp.Sge
					) e1' e2' "tmp" builder in
				let flt_bopr opr = 
					(match opr with
					  A.Add		-> L.build_fadd
					| A.Sub		-> L.build_fsub
					| A.Mult	-> L.build_fmul
					| A.Div		-> L.build_fdiv
					| A.Mod		-> L.build_frem
					| A.Equal	-> L.build_fcmp L.Fcmp.Oeq
					| A.Neq		-> L.build_fcmp L.Fcmp.One
					| A.Less	-> L.build_fcmp L.Fcmp.Olt
					| A.Leq		-> L.build_fcmp L.Fcmp.Ole
					| A.Greater	-> L.build_fcmp L.Fcmp.Ogt
					| A.Geq		-> L.build_fcmp L.Fcmp.Oge
					| _         -> raise (Foo "Invalid Float Operator")
					) e1' e2' "tmp" builder in
        (*
                    let othr_bopr opr =
                        (match opr with
                          A.Add		-> L.build_fadd
                        | A.Sub		-> L.build_fsub
                        | A.Mult	-> L.build_fmul
                        | A.Div		-> L.build_fdiv
                        | A.Mod		-> L.build_frem
                        | A.And		-> L.build_and
                        | A.Or		-> L.build_or
                        | A.Equal	-> L.build_icmp L.Icmp.Eq
                        | A.Neq		-> L.build_icmp L.Icmp.Ne
                        | A.Less	-> L.build_icmp L.Icmp.Slt
                        | A.Leq		-> L.build_icmp L.Icmp.Sle
                        | A.Greater	-> L.build_icmp L.Icmp.Sgt
                        | A.Geq		-> L.build_icmp L.Icmp.Sge
                        ) e1' e2' "tmp" builder in
        *)
				if (L.type_of e1' = flt_t && L.type_of e2' = flt_t) then flt_bopr op
				else int_bopr op
			| A.Unop(op, e) ->
				let e' = expr builder local_vars e in
				(match op with
				| A.Neg		-> L.build_neg e' "tmp" builder
				| A.Not		-> L.build_not e' "tmp" builder)
  			| A.Assign (s, e) -> let e' = expr builder local_vars e in 
				ignore (L.build_store e' (lookup s local_vars) builder); e'

			| A.Deref (s, e) ->
				(match e with
				| Some e1 -> let e2 = expr builder local_vars e1 in
					let e_ref = L.build_gep (lookup s local_vars) [| (L.const_int i32_t 0) ; e2 |] s builder in
					L.build_load e_ref s builder
				| None -> L.build_load (L.build_load (lookup s local_vars) s builder) s builder)
			| A.Ref s -> lookup s local_vars
			| A.RefChange (e1, e2) -> let e2' = expr builder local_vars e2 in
				(match e1 with
				| A.Deref(s, eo) -> (match eo with
					| Some e' -> let e3 = expr builder local_vars e' in
						let loc = L.build_gep (lookup s local_vars) [| L.const_int i32_t 0 ; e3 |] s builder  in
						L.build_store e2' loc builder
					| None -> 
						L.build_store e2' (L.build_load (lookup s local_vars) s builder) builder)
				| A.Ref s -> let loc = L.build_gep (lookup s local_vars) [| L.const_int i32_t 0 |] s builder in 
					L.build_store e2' loc builder
				| _ -> raise (Foo "Changing the reference of unchangeable expr"))
			| A.Call ("print", [e]) ->
				let format_str = L.build_global_stringptr "%s" "fmt" builder in
				L.build_call print_func [| format_str ; (expr builder local_vars e) |] "printf" builder
			| A.Call ("println", [e]) ->
				let format_str = L.build_global_stringptr "%s\n" "fmt" builder in
				L.build_call print_func [| format_str ; (expr builder local_vars e) |] "printf" builder
			| A.Call ("sprint", e) ->
				(match e with 
				| hd :: tl ->
					let format_str =  
						match hd with
						| A.StringLit s -> L.build_global_stringptr s "fmt" builder
						| _ -> L.build_global_stringptr "" "fmt" builder (*Shouldnt be hit, how do handle? *) in 
					let buf = L.build_alloca (L.array_type i8_t 4096) "buf" builder in
					let buf_ref = L.build_in_bounds_gep buf [| (L.const_int i32_t 0); (L.const_int i8_t 0) |] "buf" builder in
					let arg_arr = Array.append [| buf_ref; format_str |] (Array.of_list (List.map (fun arg -> expr builder local_vars arg) tl)) in
					ignore (L.build_call sprint_func arg_arr "sprintf" builder); buf_ref
				| _ ->	
					L.build_global_stringptr "" "fmt" builder) (*Shouldnt be hit, how do handle? *) 
			| A.Call ("input", []) ->
				let buf = L.build_alloca (L.array_type i8_t 4096) "buf" builder in 
				let buf_ref = L.build_in_bounds_gep buf [| (L.const_int i32_t 0) ; (L.const_int i8_t 0) |] "buf" builder in
				let raw_in = L.build_call input_func [| buf_ref |] "gets" builder in
				raw_in
			| A.Call ("atoi", [e]) -> L.build_call atoi_func [| expr builder local_vars e |] "atoi" builder
			| A.Call ("srand", []) -> 
				let t_holder = L.build_alloca i32_t "t_holder" builder in 
				ignore(L.build_call time_func [| t_holder |] "time" builder); 
				L.build_call srand_func [| (L.build_load t_holder "tm" builder) |] "srand" builder
			| A.Call ("rand", []) ->
				L.build_call rand_func [| |] "rand" builder
			| A.Call (f, act) ->
				let (fdef, fdecl) = StringMap.find f function_decls in
				let actuals = List.rev (List.map (expr builder local_vars) (List.rev act)) in
				let result = (match fdecl.A.typ with A.Void -> "" 
					| _ -> f ^ "_result") in
				L.build_call fdef (Array.of_list actuals) result builder 
			| A.ArrayLit el ->
				let elements = List.rev (List.map (expr builder local_vars) (List.rev el)) in
				let t = L.type_of (List.hd elements) in
				L.const_array t (Array.of_list elements)
			| A.Access (s, e) -> let e' = expr builder local_vars e in 
				let e_ref = L.build_gep (lookup s local_vars) [| (L.const_int i32_t 0) ; e' |] s builder in
				L.build_load e_ref s builder 
			in

		let add_terminal builder f =
			match L.block_terminator (L.insertion_block builder) with 
				  Some _ -> ()
				| None -> ignore (f builder) in

		let rec stmt builder local_vars = function
			| A.Block sl -> List.fold_left (fun (b, lv) s -> stmt b lv s) (builder, local_vars) sl
			| A.VDecl (t, n) ->
				let local_var = L.build_alloca (ltype_of_typ t) n builder in
				let local_vars = StringMap.add n local_var local_vars in
				(builder, local_vars)
			| A.VDeclAss (t, n, e) ->
				let local_var = L.build_alloca (ltype_of_typ t) n builder in
				let local_vars = StringMap.add n local_var local_vars in
				ignore (expr builder local_vars (A.Assign (n, e))); (builder, local_vars)
			| A.Expr e -> ignore (expr builder local_vars e); (builder, local_vars) 
			| A.Return e -> ignore (match fdecl.A.typ with
				A.Void -> L.build_ret_void builder 
					| _ -> L.build_ret (expr builder local_vars e) builder); (builder, local_vars)
			| A.If (predicate, then_stmt, else_stmt) ->
				let bool_val = expr builder local_vars predicate in
				let merge_bb = L.append_block context "merge" the_function in

				let then_bb = L.append_block context "then" the_function in
				add_terminal (fst (stmt (L.builder_at_end context then_bb) local_vars then_stmt)) 
				(L.build_br merge_bb);

				let else_bb = L.append_block context "else" the_function in
				add_terminal (fst (stmt (L.builder_at_end context else_bb) local_vars else_stmt))
				(L.build_br merge_bb);

				ignore (L.build_cond_br bool_val then_bb else_bb builder);
				(L.builder_at_end context merge_bb, local_vars)

			| A.While (predicate, body) ->
				let pred_bb = L.append_block context "for_condition" the_function in
				ignore (L.build_br pred_bb builder);

				let body_bb = L.append_block context "for_body" the_function in
				add_terminal (fst (stmt (L.builder_at_end context body_bb) local_vars body))
				(L.build_br pred_bb);

				let pred_builder = L.builder_at_end context pred_bb in
				let bool_val = expr pred_builder local_vars predicate in

				let merge_bb = L.append_block context "merge" the_function in
				ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
				(L.builder_at_end context merge_bb, local_vars)

			| A.For (e1, e2, e3, body) -> stmt builder local_vars
				( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] ) in

 		(* Build code for each statement in the function *)	
		let (builder, _) = stmt builder local_vars (A.Block fdecl.A.body) in

		(* Add a return if last block falls off end *)
		add_terminal builder (match fdecl.A.typ with 
			  A.Void -> L.build_ret_void
			| t -> L.build_ret (L.const_int (ltype_of_typ t) 0)) 
		in
	
	List.iter build_function_body program.A.functions;
	the_module
