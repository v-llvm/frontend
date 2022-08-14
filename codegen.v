module main

import v.parser
import v.ast

import v_llvm.bindings.core

pub fn codegen(text string) string {
	the_context := core.llvm_context_create()
	builder := core.llvm_create_builder_in_context(the_context)
	
	the_module := core.llvm_module_create_with_name_in_context("vlang", the_context)

	func_args := [core.llvm_int32_type(), core.llvm_int32_type()]
	func_ty := core.llvm_function_type(core.llvm_int32_type(), func_args, false)
	the_function := core.llvm_add_function(the_module, "my_func", func_ty)

	return core.llvm_print_module_to_string(the_module)
}

