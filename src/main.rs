#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate combine;

pub mod parser;
pub mod compiler;

use std::env::args;
use std::io::{Read, BufRead, BufReader, stdout};
use std::fs::File;

fn main() {
    let path = args().nth(1).expect(".jds file");
    let file = File::open(path).expect("file exits");
    let mut br = BufReader::new(file);
    let mut input = String::new();
    br.read_to_string(&mut input).expect("read succeed");

    let ast = parser::parse(&input).expect("correct syntax");
    let js = compiler::compile(ast);

    serde_json::to_writer_pretty(stdout(), &js).unwrap();
}
