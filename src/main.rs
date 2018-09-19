#![recursion_limit = "128"]
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;
#[macro_use]
extern crate combine;
#[macro_use]
extern crate structopt_derive;
extern crate regex;
extern crate structopt;
#[macro_use]
extern crate lazy_static;

pub mod compiler;
pub mod formatter;
pub mod parser;

use structopt::clap::{Error, ErrorKind};
use structopt::StructOpt;

use std::fs::File;
use std::io::{BufReader, Read};
use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum Format {
    Json,
    Yaml,
}

impl FromStr for Format {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "json" => Ok(Format::Json),
            "yaml" => Ok(Format::Yaml),
            _ => Err(Error {
                message: "yaml|json".into(),
                kind: ErrorKind::InvalidValue,
                info: None,
            }),
        }
    }
}

#[derive(Debug, Clone, StructOpt)]
pub struct Config {
    #[structopt(
        long = "path-prefix",
        help = "path prefix of paths",
        default_value = "/definitions"
    )]
    pub path_prefix: String,
    #[structopt(
        long = "no-swagger",
        help = "don't use swagger spesific notation"
    )]
    pub no_swagger: bool,
    #[structopt(
        long = "format",
        help = "output format (json|yaml)",
        default_value = "json"
    )]
    pub format: Format,
    #[structopt(long = "pack", help = "if pack the output")]
    pub pack: bool,
    #[structopt(help = "input file")]
    pub input: String,
}

fn main() {
    let config = Config::from_args();
    debug!("CLI options: {:?}", config);
    let file = File::open(&config.input).expect("file exits");
    let mut br = BufReader::new(file);
    let mut input = String::new();
    br.read_to_string(&mut input).expect("read succeed");

    let ast = parser::parse(&config, &input).expect("correct syntax");
    let schema = compiler::compile(&config, ast);
    formatter::format(&config, schema);
}
