use std::fs;
use structopt::StructOpt;
use std::env;

use homework3::compiler::*;

#[derive(Debug, StructOpt)]
#[structopt(name = "Custom Simple Compiler", about = "A simple compiler")]
struct Opt {
    #[structopt(short = "i", long = "input", help = "Sets the input source file path")]
    input: String,

    #[structopt(short = "o", long = "output", help = "Sets the output assembly file path")]
    output: String,
}

fn main() {
	env::set_var("RUST_BACKTRACE", "1");

	let opt = Opt::from_args();

	let code = fs::read_to_string(&opt.input).unwrap();
	println!("Program: \n{}", code);

	let tokens = Compiler::lex(code);
	println!("\nTokens: ");
	for token in &tokens {
		println!("{}", token);
	}

	let ast = Compiler::parse(tokens);
    let mut ast_visualize_data = String::new();
	ast.iter_vertices().for_each(|v| {
		ast_visualize_data.push_str(&format!("v,{},{},{}\n", v.uid, v.construct_type, v.content));
    });

    ast_visualize_data.push_str("\n");

	ast.iter_vertices().for_each(|v| {
        ast.iter_targets(v).unwrap().for_each(|(e, t)| {
        	ast_visualize_data.push_str(&format!("e,{},{},{}\n", v.uid, t.uid, e));
        });
    });

    fs::write("temp_tree_visualize_data", &ast_visualize_data).unwrap();

    println!("\nAbstract syntax tree: ");
	println!("ast {{");
	ast.iter_vertices().for_each(|v| {
		println!("    {} => {{", v);
        ast.iter_targets(v).unwrap().for_each(|(e, t)| {
        	println!("        {} -> {} ({}),", v, t, e);
        });
        println!("  }}");
    });
    println!("}}");

    let assembly = Compiler::generate(ast);
    println!("\nAssembly: \n{}\n", assembly);

    fs::write(&opt.output, &assembly).unwrap();
}
