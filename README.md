[![Build Status](https://travis-ci.org/KeenS/chema.svg?branch=master)](https://travis-ci.org/KeenS/chema)
[![Build status](https://ci.appveyor.com/api/projects/status/3o96tvfb0wv597ud/branch/master?svg=true)](https://ci.appveyor.com/project/KeenS/chema/branch/master)

[![chema at crates.io](https://img.shields.io/crates/v/chema.svg)](https://crates.io/crates/chema)

# Chema

Generate JSON Schema from a lightweight DSL.

This is originally intended to generate a `definitions` section of swagger specifications.

# Install

Download a binary from https://github.com/KeenS/chema/releases
or if you have setup `cargo`, use `cargo install like below.

```
$ cargo install chema
```

# Usage

```
chema 0.0.8
Sunrin SHIMURA (keen) <3han5chou7@gmail.com>
An external DSL for JSON Schema
USAGE:
    chema [FLAGS] [OPTIONS] <input>
FLAGS:
    -h, --help          Prints help information
        --no-swagger    don't use swagger specific notation
        --pack          if pack the output
    -V, --version       Prints version information
OPTIONS:
        --format <format>              output format (json|yaml) [default: json]
        --path-prefix <path_prefix>    path prefix of paths [default: /definitions]
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
     | IDENT | "[" TYPE "]" | STRUCT | ENUM | TYPE "?"
     | "format" "(" STRING ")" | "url" "(" STRING ")"
     | TYPE "&" TYPE |  TYPE "|" TYPE
     | TYPE "where" PRED
     | "(" TYPE ")" | STRING

STRUCT = "struct" "{" (FIELD ",")+ "}"
FIELD = IDENT "?"? ":" TYPE

ENUM = "enum" "{" (VARIANT",")+ "}"
VARIANT = STRING

PRED = UNUMBER "<=" "length" | "length" <= UNUMBER
     | "format" "=" STRING | "it" "=~" REGEX
     | PRED && PRED

IDENT = [a-zA-Z_][a-zA-Z0-9_]*
STRIING = "\"" ([^"\\]|\\.)* "\""
REGEX   = "/" ([^/\\]|\\.)* "/"
UNUMBER = [0-9]+

COMMENT = "//" any "\n" | "/*" any "*/"
DOC_COMMENT = "/**" any "*/"
```

# Example

See [etc](etc). `*.jsd`s are the sources and `*.jsons` are the generated files.

# Supported Platforms

UNIX-like system will be supported.
Ubuntu LTS is the major target.
Windows support is best effort and may not work .
