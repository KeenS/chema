[![Build Status](https://travis-ci.org/KeenS/chema.svg?branch=master)](https://travis-ci.org/KeenS/chema)
[![chema at crates.io](https://img.shields.io/crates/v/chema.svg)](https://crates.io/crates/chema)

# Chema
Generate JSON Schema from a lightweight DSL.

This is originally intended to generate a `definitions` section of swagger specifications

# Install

Download a binary from https://github.com/KeenS/chema/releases
or if you have setup `cargo`, use `cargo install like below

```
$ cargo install chema
```

# Usage

```
chema 0.0.2
Sunrin SHIMURA (keen) <3han5chou7@gmail.com>
An external DSL for JSON Schema

USAGE:
    chema [FLAGS] [OPTIONS] <input>

FLAGS:
    -h, --help          Prints help information
        --no-swagger    don't use swagger spesific notation (e.g. nullable)
        --pack          pack (unprettify) the output
    -V, --version       Prints version information

OPTIONS:
        --format <format>    output format (json|yaml) [default: json]

ARGS:
    <input>    input file
```
# Syntax

```
TOP = ITEMS
ITEMS = ITEM+
ITEM = TYPEDEF

TYPEDEF = "type" IDENT "=" TYPE ";"

TYPE = "null" | "boolean" | "object" | "number" | "string" | "integer"
     | IDENT | "[" TYPE "]" | STRUCT | ENUM | TYPE "?" | "format" "(" STRING ")"
     | TYPE "&" TYPE |  TYPE "|" TYPE | "(" TYPE ")"

STRUCT = "struct" "{" (FIELD ",")+ "}"
FIELD = IDENT ":" TYPE

ENUM = "enum" "{" (VARIANT",")+ "}"
VARIANT = STRING

IDENT = [a-zA-Z_][a-zA-Z0-9_]*
STRIING = "\"" ([^"\\]|\.)* "\""

COMMENT = "//" any "\n" | "/*" any "*/"
DOC_COMMENT = "/**" any "*/"
```

# Example

See [etc](etc). `*.jsd`s are the sources and `*.jsons` are the generated files.
