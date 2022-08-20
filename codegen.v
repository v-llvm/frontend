module main

import v.ast
import v.builder
import v.pref
import v_llvm.bindings.analysis
import v_llvm.bindings.core
import v_llvm.bindings.types

pub fn codegen(path string) string {
	mut pref := &pref.Preferences{vroot: '/usr/lib/vlang', path: path} // temp fix
	pref.fill_with_defaults()
	mut b := builder.new_builder(pref)
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()

	// Parse files
	b.front_and_middle_stages(files) or { panic('front or middle failed') }

	// Process parsed files
	generated := codegen_file(b, b.parsed_files) or { '' }
	$if trace ? { println('trace: println(generated)') }
	println(generated)
	return generated
}

fn codegen_file(b builder.Builder, files []&ast.File) ?string {
	// Create builder and top-level module
	the_context := core.llvm_context_create()
	the_module := core.llvm_module_create_with_name_in_context(files.last().mod.name, the_context)
	builder := core.llvm_create_builder_in_context(the_context)

	/* Generate llvm definitions from ast table*/
	mut lookup := &Lookup{table: b.table}
	
	// Generate builtin types
	lookup.types[ast.void_type] = core.llvm_void_type_in_context(the_context)
	lookup.types[ast.i8_type] = core.llvm_int8_type_in_context(the_context)
	lookup.types[ast.i16_type] = core.llvm_int16_type_in_context(the_context)
	lookup.types[ast.int_type] = core.llvm_int32_type_in_context(the_context)
	lookup.types[ast.i64_type] = core.llvm_int64_type_in_context(the_context)
	lookup.types[ast.isize_type] = lookup.types[ast.i64_type]
	lookup.types[ast.u8_type] = lookup.types[ast.i8_type]
	lookup.types[ast.u16_type] = lookup.types[ast.i16_type]
	lookup.types[ast.u32_type] = lookup.types[ast.int_type]
	lookup.types[ast.u64_type] = lookup.types[ast.i64_type]
	lookup.types[ast.usize_type] = lookup.types[ast.u64_type]
	lookup.types[ast.f32_type] = core.llvm_float_type_in_context(the_context)
	lookup.types[ast.f64_type] = core.llvm_double_type_in_context(the_context)
	lookup.types[ast.char_type] = lookup.types[ast.i8_type]
	lookup.types[ast.bool_type] = core.llvm_int1_type_in_context(the_context)
	lookup.types[ast.voidptr_type] = core.llvm_pointer_type(lookup.types[ast.void_type], 0)
	lookup.types[ast.byteptr_type] = core.llvm_pointer_type(lookup.types[ast.u8_type], 0)
	lookup.types[ast.charptr_type] = core.llvm_pointer_type(lookup.types[ast.char_type], 0)

	// Generate non-primitive types
	for sym in b.table.type_symbols {
		if sym.idx in lookup.types { continue }
		match sym.kind {
			.alias {
				lookup.types[sym.idx] = lookup.resolve_type((sym.info as ast.Alias).parent_type)?
			}
			.array {
				info := sym.array_info()
				assert info.nr_dims == 1
				lookup.types[sym.idx] = core.llvm_struct_create_named(the_context, sym.name)
				lookup.structs[sym.name] = lookup.types[sym.idx]
			}
			.array_fixed {
				// info := sym.array_fixed_info()
				// assert info.elem_type.idx() in lookup.types.keys()
				// lookup.types[sym.idx] = core.llvm_pointer_type(lookup.types[info.elem_type.idx()], 0)
				lookup.types[sym.idx] = lookup.types[ast.voidptr_type]
			}
			.string {
				lookup.types[sym.idx] = core.llvm_struct_create_named(the_context, sym.name)
				lookup.structs[sym.name] = lookup.types[sym.idx]
			}
			.struct_ {
				lookup.types[sym.idx] = core.llvm_struct_create_named(the_context, sym.name)
				lookup.structs[sym.name] = lookup.types[sym.idx]
			}
			else {
				lookup.types[sym.idx] = lookup.types[ast.voidptr_type]
				eprintln("warning: symbol type $sym.kind for $sym.name is not implemented")
			}
		}
	}

	/*for name, idx in b.table.type_idxs {
		if idx in lookup.types { continue }
		lookup.types[idx] = core.llvm_struct_create_named(the_context, name)
		lookup.structs[name] = lookup.types[idx]
	}*/

	// Add function declarations for all used functions
	for name, v_fn in b.table.fns {
		mut func_arg_tys := []types.LLVMTypeRef{}
		for p in v_fn.params {
			func_arg_tys << lookup.resolve_type(p.typ) or { panic('') }
		}
		func_return_ty := lookup.resolve_type(v_fn.return_type) or { panic('') }
		func_ty := core.llvm_function_type(func_return_ty, func_arg_tys, false)
		lookup.fns[name] = core.llvm_add_function(the_module, name, func_ty)
	}

	// Create generator and visit root node
	mut g := Generator{
		lookup: lookup
		ctx: the_context
		mod: the_module
		builder: builder
	}
	for file in files {
		for stmt in file.stmts {
			println("visiting: $stmt")
			g.visit(&stmt) or { /* No expression returned */ }
		}
	}
	// verify module
	$if trace ? { println('trace: analysis.llvm_verify_module(the_module, .llvm_print_message_action)') }
	if analysis.llvm_verify_module(the_module, .llvm_print_message_action, unsafe { nil }) {
		panic('')
	}
	// print module to string
	$if trace ? { println('trace: core.llvm_print_module_to_string(the_module)') }
	return core.llvm_print_module_to_string(the_module)
}

struct Lookup {
	table	&ast.Table
mut:
	types	map[int]types.LLVMTypeRef
	structs	map[string]types.LLVMTypeRef
	fns		map[string]types.LLVMValueRef
	vars	map[string]types.LLVMValueRef
}

fn (l Lookup) resolve_type(t &ast.Type) ?types.LLVMTypeRef {
	if t.idx() !in l.types {
		t_str := l.table.type_to_str(t)
		t_kind := l.table.type_kind(t)
		eprintln('warning: $t_kind $t_str is not registered')
		return none
	}
	mut llvm_type := l.types[t.idx()]
	for _ in 0..t.nr_muls() {
		llvm_type = core.llvm_pointer_type(llvm_type, 0)
	}
	return llvm_type
}

struct Generator {
	ctx     types.LLVMContextRef
	mod     types.LLVMModuleRef
	builder types.LLVMBuilderRef
mut:	
	lookup  &Lookup
}

fn (mut g Generator) visit(s &ast.Stmt) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit($s)") }
	match s {
		// Top-level
		ast.Module { /* TODO */ }
		ast.Import { /* TODO */ }
		ast.HashStmt { /* TODO */ }
		ast.FnDecl { return g.visit_fn_decl(s) }
		ast.StructDecl { return g.visit_struct_decl(s) }
		ast.ForCStmt { return g.visit_for_c_stmt(s) }
		ast.ForInStmt { return g.visit_for_in_stmt(s) }
		ast.Return { return g.visit_return(s) }
		ast.TypeDecl { return g.visit_type_decl(s) }
		// Bodies
		ast.AssignStmt { return g.visit_assign_stmt(s) }
		ast.ExprStmt { return g.visit_expr_stmt(s) }
		// Unimplemented
		else { eprintln('warning: no implementation for statement $s.type_name()') }
	}
	return none
}

fn (mut g Generator) visit_fn_decl(s &ast.FnDecl) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_fn_decl($s)") }
	// Get function declaration
	func := core.llvm_get_named_function(g.mod, s.name)
	if func == unsafe { nil } {
		println("warning: function $s.name is not registered")
		return none
	}
	// create entry basic block
	entry := core.llvm_append_basic_block_in_context(g.ctx, func, 'entry')
	// set the insertion point of the builder to the end of this basic block
	core.llvm_position_builder_at_end(g.builder, entry)
	// generate code for the args of the function
	for i, param in s.params {
		// generate variable definition
		assert param.typ.idx() in g.lookup.types.keys()
		arg_ty := g.lookup.resolve_type(param.typ)?
		println(param)
		if param.typ.is_ptr() || true {
			// TODO
			continue
		}
		// TODO: This cant be an opaque type
		$if trace ? {
			arg_ty_str := core.llvm_print_type_to_string(arg_ty)
			println("trace: core.llvm_build_alloca(g.builder, $arg_ty_str, var_$param.name)")
		}
		alloca := g.create_alloca(arg_ty, 'var_$param.name')
		g.lookup.vars[param.name] = alloca
		// assign value
		assert i < core.llvm_count_params(func)
		$if trace ? {
			println("trace: core.llvm_get_param(func, $i)")
		}
		arg_op := core.llvm_get_param(func, u32(i))
		core.llvm_build_store(g.builder, arg_op, alloca)
	}
	// generate code for the body of the function
	for c in s.stmts {
		g.visit(&c) or { /* Ignore */ }
	}
	// terminate basic block
	$if trace ? { println("trace: core.llvm_get_insert_block(g.builder)") }
	insert_block := core.llvm_get_insert_block(g.builder)
	$if trace ? { println("trace: core.llvm_get_basic_block_terminator(insert_block)") }
	terminator := core.llvm_get_basic_block_terminator(insert_block)
	if terminator == unsafe { nil } {
		// TODO: nonvoid return
		func_ty := core.llvm_type_of(func)
		mut ret_ty := core.llvm_get_return_type(func_ty)
		if core.llvm_get_type_kind(ret_ty) == .llvm_function_type_kind {
			// weird fix but works
			ret_ty = core.llvm_get_return_type(ret_ty)
		}

		if core.llvm_get_type_kind(ret_ty) == .llvm_void_type_kind {
			$if trace ? { println("trace: core.llvm_build_ret_void(g.builder)") }
			core.llvm_build_ret_void(g.builder)
		}
		else if core.llvm_get_type_kind(ret_ty) == .llvm_pointer_type_kind {
			$if trace ? { println("trace: core.llvm_build_ret(g.builder, core.llvm_const_pointer_null(ret_ty))") }
			core.llvm_build_ret(g.builder, core.llvm_const_pointer_null(ret_ty))
		}
		else {
			$if trace ? { println("trace: core.llvm_build_ret(g.builder, core.llvm_const_null(ret_ty))") }
			core.llvm_build_ret(g.builder, core.llvm_const_null(ret_ty))
		}
	}
	// assert core.llvm_get_basic_block_terminator(insert_block) != unsafe { nil }
	// verify function
	$if trace ? { println("trace: analysis.llvm_verify_function(func, .llvm_print_message_action)") }
	if analysis.llvm_verify_function(func, .llvm_print_message_action) {
		panic('')
	}
	return none
}

fn (g Generator) visit_struct_decl(s &ast.StructDecl) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_struct_decl($s)") }
	llvm_struct := g.lookup.structs[s.name]
	mut element_types := []types.LLVMTypeRef{}
	for f in s.fields {
		element_types << g.lookup.resolve_type(f.typ)?
	}
	core.llvm_struct_set_body(llvm_struct, element_types, false)
	return none
}

fn (mut g Generator) visit_assign_stmt(s &ast.AssignStmt) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_assign_stmt($s)") }
	for i, left in s.left {
		right := g.visit_expr(&s.right[i])?
		assert s.left_types[i].idx() in g.lookup.types.keys()
		assert s.right_types[i].idx() in g.lookup.types.keys()
		left_type, right_type := g.lookup.types[s.left_types[i].idx()], g.lookup.types[s.right_types[i].idx()]
		// TODO: assert types (conversion?)
		if left !is ast.Ident { panic('cannot assign to non-identifier') }
		match s.op {
			.decl_assign {
				name := (left as ast.Ident).name
				if core.llvm_get_type_kind(left_type) == .llvm_pointer_type_kind {
					alloca := core.llvm_build_alloca(g.builder, g.lookup.resolve_type(ast.i8_type)?, name)
					return alloca // TODO
				}
				alloca := core.llvm_build_alloca(g.builder, left_type, name)
				g.lookup.vars[name] = alloca
				core.llvm_build_store(g.builder, right, alloca)
				return alloca
			}
			else { panic('warning: no implementation for assignment $s.op') }
		}
	}
	return none
}

fn (g Generator) visit_for_c_stmt(s &ast.ForCStmt) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_for_c_stmt($s)") }
	eprintln('warning: no implementation for ForCStmt')
	return core.llvm_const_int_of_string(g.lookup.types[ast.int_type], "0", 10)
}

fn (g Generator) visit_for_in_stmt(s &ast.ForInStmt) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_for_in_stmt($s)") }
	eprintln('warning: no implementation for ForInStmt')
	return core.llvm_const_int_of_string(g.lookup.types[ast.int_type], "0", 10)
}

fn (g Generator) visit_return(s &ast.Return) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_return($s)") }
	// TODO: support multireturn
	ret_val := g.visit_expr(&s.exprs[0])?
	// ret_ty := g.lookup.resolve_type(s.types[0])?
	return core.llvm_build_ret(g.builder, ret_val)
}

fn (g Generator) visit_type_decl(t ast.TypeDecl) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_type_decl($t)") }
	match t {
		ast.AliasTypeDecl { return g.visit_alias_type_decl(t) }
		// Unimplemented
		else { eprintln('warning: no implementation for type declaration $t.type_name()') }
	}
	return none
}

fn (g Generator) visit_alias_type_decl(t &ast.AliasTypeDecl) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_alias_type_decl($t)") }
	eprintln('warning: no implementation for AliasTypeDecl')
	// TODO: This isn't correct, resolve correct type
	println(core.llvm_print_type_to_string(g.lookup.types[t.parent_type]))
	return none
}

fn (g Generator) visit_expr_stmt(s &ast.ExprStmt) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_expr_stmt($s)") }
	// TODO assert type
	return g.visit_expr(&s.expr)
}

fn (g Generator) visit_expr(e &ast.Expr) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_expr($e)") }
	match e {
		ast.ArrayInit { return g.visit_array_init(e) }
		ast.Comment { /* Swallow */ }
		ast.CallExpr { return g.visit_call_expr(e) }
		ast.Ident { return g.visit_ident(e) }
		ast.IfExpr { return g.visit_if_expr(e) }
		ast.IntegerLiteral { return g.visit_integer_literal(e) }
		ast.IndexExpr { return g.visit_index_expr(e) }
		ast.MatchExpr { return g.visit_match_expr(e) }
		ast.StringLiteral { return g.visit_string_literal(e) }
		// Unimplemented
		else { eprintln('warning: no implementation for expression $e.type_name()') }
	}
	return none
}

fn (g Generator) visit_array_init(e &ast.ArrayInit) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_array_init($e)") }
	eprintln('warning: no implementation for ArrayInit')
	return core.llvm_const_int_of_string(g.lookup.types[ast.int_type], "0", 10)
}

fn (g Generator) visit_call_expr(e &ast.CallExpr) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_call_expr($e)") }
	// Get function declaration
	func := core.llvm_get_named_function(g.mod, e.name)
	assert e.return_type.idx() in g.lookup.types.keys()
	ret_ty := g.lookup.types[e.return_type.idx()]
	// Compose arguments
	mut args := []types.LLVMValueRef{}
	for a in e.args { args << g.visit_expr(&a.expr)? }
	// Provide variable name if not void
	name := if core.llvm_get_type_kind(ret_ty) == .llvm_void_type_kind { "" } else { "calltemp" }
	// Call function
	return core.llvm_build_call2(g.builder, ret_ty, func, args, name)
}

fn (g Generator) visit_ident(e &ast.Ident) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_ident($e)") }
	// TODO: temp fix
	if e.name !in g.lookup.vars.keys() {
		return none
	}
	assert e.name in g.lookup.vars.keys()
	alloca := g.lookup.vars[e.name]
	ptr_ty := core.llvm_get_allocated_type(alloca)
	if core.llvm_get_type_kind(ptr_ty) == .llvm_pointer_type_kind { return none } // TODO
	return core.llvm_build_load2(g.builder, ptr_ty, alloca, e.name)
}

fn (g Generator) visit_if_expr(e &ast.IfExpr) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_if_expr($e)") }
	eprintln('warning: no implementation for IfExpr')
	return core.llvm_const_int_of_string(g.lookup.types[ast.int_type], "0", 10)
}

fn (g Generator) visit_integer_literal(e &ast.IntegerLiteral) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_integer_literal($e)") }
	return core.llvm_const_int_of_string(g.lookup.types[ast.int_type], e.val, 10)
}

fn (g Generator) visit_index_expr(e &ast.IndexExpr) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_index_expr($e)") }
	eprintln('warning: no implementation for IndexExpr')
	return core.llvm_const_int_of_string(g.lookup.types[ast.int_type], "0", 10)
}

fn (g Generator) visit_match_expr(e &ast.MatchExpr) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_match_expr($e)") }
	eprintln('warning: no implementation for MatchExpr')
	return core.llvm_const_int_of_string(g.lookup.types[ast.int_type], "0", 10)
}

fn (g Generator) visit_string_literal(e &ast.StringLiteral) ?types.LLVMValueRef {
	$if trace ? { println("trace: visit_string_literal($e)") }
	// allocate and assign
	alloca := g.create_alloca(g.lookup.structs["string"], "const")
	str_ptr := core.llvm_build_struct_gep2(g.builder, g.lookup.types[ast.charptr_type], alloca, 0, "str")
	core.llvm_build_store(g.builder, core.llvm_const_string_in_context(g.ctx, e.val, false), str_ptr)
	len_ptr := core.llvm_build_struct_gep2(g.builder, g.lookup.types[ast.int_type], alloca, 1, "len")
	core.llvm_build_store(g.builder, core.llvm_const_int(g.lookup.types[ast.int_type], e.val.len, false), len_ptr)
	return alloca
}

fn (g Generator) create_alloca(t types.LLVMTypeRef, name string) types.LLVMValueRef {
	return core.llvm_build_alloca(g.builder, t, name)
}

fn (g Generator) create_array_alloca(t types.LLVMTypeRef, v types.LLVMValueRef, name string) types.LLVMValueRef {
	return core.llvm_build_array_alloca(g.builder, t, v, name)
}
