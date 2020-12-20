extern crate interp;

use interp::frontend::{
	// token::*, lexer::*,
	parser::*
};

fn main () {
	const TEST_FILE: &str = "./test_scripts/basic_pegs.z";

	let test_file = match std::fs::read_to_string(TEST_FILE) {
		Ok(s) => s,
		Err(e) => panic!("Failed to read file at path {}: {}", TEST_FILE, e)
	};


	// let tokens: Vec<Token> = test_file.lex().collect();
	// println!("{:#?}", tokens);


	let mut parser = test_file.syn();

	match parser.items() {
		Value(v) => println!("{:?}", v),
		Problem(e) => println!("{}", e.display(TEST_FILE)),
		Nothing => println!("Parsed nothing")
	}

	assert!(parser.is_finished(), "Failed to parse all tokens")
}