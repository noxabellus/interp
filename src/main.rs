extern crate interp;


fn main () {
  const TEST_FILE: &str = "./test_scripts/basic_pegs.z";

  let test_file = match std::fs::read_to_string(TEST_FILE) { Ok(s) => s, Err(e) => panic!("Failed to read file at path {}: {}", TEST_FILE, e) };

  use interp::frontend::parser::*;
  use std::panic;

  let mut parser = ParserIter::new(test_file.as_str());

  let expr = expr(&mut parser);

  match expr {
    Value(expr) => println!("Parsed expr: {:#?}", expr),
    Problem(e) => println!("{}", parser.display_error(TEST_FILE, e)),
    Nothing => println!("Parsed nothing")
  }
}