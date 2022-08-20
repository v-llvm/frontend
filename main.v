module main

import flag
import os
import time

fn main() {
	// Command flags
	mut fp := flag.new_flag_parser(os.args)
	fp.skip_executable()
	fp.limit_free_args_to_exactly(1)?
	// Usage
	fp.application('v llvm')
	fp.description('Generate LLVM IR from a V source.')
	fp.usage_example('-							generate LLVM IR from stdin and print to stdout.')
	fp.usage_example('example.v					generate LLVM IR from example.v and print to stdout.')
	fp.usage_example('-oexample.ll -			generate LLVM IR from stdin and write to example.ll.')
	fp.usage_example('-oexample.ll example.v	generate LLVM IR from example.v and write to example.ll')
	// Parse options
	output_filename := fp.string('output', `o`, '-', 'filename to write the output to')
	// Parse remaining parameters
	input_filename := fp.finalize()?[0] or { '-' }
	// // Read input
	// input_text := input_read(input_filename)
	// Generate LLVM IR
	mut timer := time.StopWatch{}
	timer.start()
	// output_text := codegen(input_text)
	output_text := codegen(input_filename)
	timer.stop()
	// Write output
	output_write(output_filename, output_text)
	// Print stats to stderr
	eprintln('written LLVM IR to "$output_filename" in $timer.elapsed().str()')
}

/*
fn input_read(filename string) string {
	if filename == '-' {
		return os.get_raw_lines_joined()
	}
	if os.file_ext(filename) !in ['.v', '.vv', '.vsh'] {
		panic('"$filename" must be a v or vsh file')
	}
	if !os.exists(filename) {
		panic('"$filename" does not exist')
	}
	return os.read_file(filename) or { panic('failed to read "$filename"') }
}
*/

fn output_write(filename string, text string) {
	if filename == '-' {
		print(text)
		flush_stdout()
		return
	}
	os.write_file(filename, text) or { panic('failed to write "$filename"') }
}
