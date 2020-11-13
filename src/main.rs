extern crate interp;

use interp::frontend::parser::*;

fn main () {
  const TEST_FILE: &str = "./test_scripts/basic_pegs.z";

  let test_file = match std::fs::read_to_string(TEST_FILE) {
    Ok(s) => s,
    Err(e) => panic!("Failed to read file at path {}: {}", TEST_FILE, e)
  };  

  let mut parser = test_file.syn();

  match parser.expr() {
    Value(v) => println!("Parsed expr: {:#?}", v),
    Problem(e) => println!("{}", parser.display_error(TEST_FILE, e)),
    Nothing => println!("Parsed nothing")
  }
}