use std::{
	io::{ self, Write },
	fs::File,
};


extern crate zeta;
use zeta::{
	vm::*,
	frontend::{
		token::*,
		lexer::*,
		parser::*,
		analyzer::*, 
	}
};

fn main () -> io::Result<()> {
	const TEST_FILE: &str = "./test_scripts/fold.z";

	let test_file = match std::fs::read_to_string(TEST_FILE) {
		Ok(s) => s,
		Err(e) => panic!("Failed to read file at path {}: {}", TEST_FILE, e)
	};


	
	let tokens: Vec<Token> = test_file.lex().collect();
	let mut tokens_log = File::create("./local/log/lex.ron")?;
	write!(tokens_log, "{:#?}", tokens)?;


	let mut parser = test_file.syn();

	let mut items = match parser.items() {
		Value(v) => v,
		Problem(e) => panic!("{}", e.display(TEST_FILE)),
		Nothing => panic!("Parsed nothing")
	};

	let mut items_log = File::create("./local/log/syn.ron")?;
	write!(items_log, "{:#?}", items)?;

	assert!(parser.is_finished(), "Failed to parse all tokens");

	let mut context = Context::new();
	let mut analyzer = Analyzer::new(&mut context);

	let result = analyzer.analyze(&mut items);
	
	let mut analyzer_log = File::create("./local/log/sem.ron")?;
	write!(analyzer_log, "{:#?}", analyzer)?;

	if let Err(e) = result {
		panic!("{}", e.display(TEST_FILE));
	}


	Ok(())
}