//! TOP = ITEMS
//! ITEMS = ITEM+
//! ITEM = TYPEDEF
//!
//! TYPEDEF = "type" IDENT "=" TYPE
//!
//! STRUCT = "struct" STRING? "{" (FIELD ",")+ "}"
//! FIELD = IDENT ":" TYPE
//!
//! ENUM = "enum" "{" (VARIANT",")+ "}"
//! VARIANT = STRING
//!
//! TYPE = "null" | "boolean" | "object" | "number" | "string" | "integer"
//!      | IDENT | "[" TYPE "]" | STRUCT | ENUM
//!
//! IDENT = [a-zA-Z][a-zA-Z0-9]*
//! STRIING = "" ""
//!

use combine::{Parser, ParseError, Stream};
use combine::{skip_many, skip_many1, satisfy, optional, sep_by1, sep_end_by1, try};
use combine::char::{alpha_num, letter, char, string, spaces};
use combine::combinator::recognize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AST(pub Vec<Item>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item {
    TypeDef(TypeDef),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDef {
    pub ident: Ident,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub title: Option<String>,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub ident: Ident,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant(pub String);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Null,
    Boolean,
    Object,
    Number,
    String,
    Integer,
    Ident(Ident),
    Array(Box<Type>),
    Struct(Struct),
    Enum(Enum),
    Option(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(pub String);


pub fn parse(input: &str) -> Result<AST, ParseError<&str>> {
    ast().parse(input).map(|r| r.0)
}

parser!{
    fn ast[I]()(I) -> AST
        where [I: Stream<Item=char>]
    {
        blank().with(sep_end_by1(item(), blank())).map(AST)
    }
}

parser!{
    fn item[I]()(I) -> Item
        where [I: Stream<Item=char>]
    {
        typedef().map(Item::TypeDef)
    }
}

parser!{
    fn typedef[I]()(I) -> TypeDef
        where [I: Stream<Item=char>]
    {
        struct_parser! {
            TypeDef {
                _: string("type").skip(blank()),
                ident: ident().skip(blank()),
                _: char('=').skip(blank()),
                type_: type_().skip(blank()),
                _: char(';'),
            }
        }
    }
}

parser!{
    fn struct_[I]()(I) -> Struct
        where [I: Stream<Item=char>]
    {
        let field = (ident().skip(blank()), char(':').skip(blank()), type_())
            .map(|(ident, _, type_)| Field {ident, type_});

        struct_parser! {
            Struct {
                _: string("struct").skip(blank()),
                title: optional(str().skip(blank())),
                _: char('{').skip(blank()),
                fields: sep_by1(field.skip(blank()), char(',').skip(blank())),
                _: char('}'),
            }
        }
    }
}


parser!{
    fn enum_[I]()(I) -> Enum
        where [I: Stream<Item=char>]
    {
        let variant = str().map(Variant);

        struct_parser! {
            Enum {
                _: string("enum").skip(blank()),
                _: char('{').skip(blank()),
                variants: sep_by1(variant.skip(blank()), char(',').skip(blank())),
                _: char('}').skip(blank())
            }
        }
    }
}

parser!{
    fn type_[I]()(I) -> Type
        where [I: Stream<Item=char>]
    {

        choice!(
            try(string("null").map(|_| Type::Null)),
            try(string("boolean").map(|_| Type::Boolean)),
            try(string("object").map(|_| Type::Object)),
            try(string("number").map(|_| Type::Number)),
            try(string("string").map(|_| Type::String)),
            try(string("integer").map(|_| Type::Integer)),
            try((char('[').skip(blank()), type_(), blank().with(char(']'))).map(|(_, ty, _)| Type::Array(Box::new(ty)))),
            try(struct_().map(Type::Struct)),
            try(enum_().map(Type::Enum)),
            try(ident().map(Type::Ident))
        ).and(optional(char('?'))).map(|(ty, opt)| match opt {
            Some(_) => Type::Option(Box::new(ty)),
            None => ty
        })
    }
}

parser!{
    fn ident[I]()(I) -> Ident
        where [I: Stream<Item=char>]
    {
        recognize(letter().with(skip_many1(alpha_num())))
            .map(|s: String| Ident(s))
    }
}

parser!{
    fn str[I]()(I) -> String
        where [I: Stream<Item=char>]
    {
        // FIXME
        (char('"'), ident(), char('"')).map(|(_, ident, _)| ident.0)
    }
}

parser! {
    fn blank[I]()(I) -> ()
        where [I: Stream<Item=char>]
    {
        spaces().skip(optional(comment().skip(optional(spaces()))))
    }
}

parser! {
    fn comment[I]()(I) -> ()
        where [I: Stream<Item=char>]
    {
        (string("//"), skip_many(satisfy(|c| c != '\n'))).map(|_|())
    }

}


macro_rules! assert_parsed {
    ($parser: expr, $input: expr, $expected: expr) => {
        assert_eq!($parser.parse($input).map(|t| t.0), Ok($expected))
    }
}

macro_rules! assert_parse_fail {
    ($parser: expr, $input: expr) => {
        assert!($parser.parse($input).is_err())
    }
}

#[test]
fn test_ident() {
    assert_parsed!(ident(), "ident1", Ident("ident1".into()));

    assert_parsed!(ident(), "ident1  ", Ident("ident1".into()));

    assert_parse_fail!(ident(), "0ident1  ");
}

#[test]
fn test_type() {
    assert_parsed!(type_(), "null", Type::Null);
    assert_parsed!(type_(), "boolean", Type::Boolean);
    assert_parsed!(type_(), "object", Type::Object);
    assert_parsed!(type_(), "number", Type::Number);
    assert_parsed!(type_(), "string", Type::String);
    assert_parsed!(type_(), "integer", Type::Integer);
    assert_parsed!(type_(), "user", Type::Ident(Ident("user".into())));
    assert_parsed!(type_(), "[string]", Type::Array(Box::new(Type::String)));
    assert_parsed!(
        type_(),
        "[[string]]",
        Type::Array(Box::new(Type::Array(Box::new(Type::String))))
    );
    assert_parsed!(
        type_(),
        "struct {id: integer, name: string}",
        Type::Struct(Struct {
            title: None,
            fields: vec![
                Field {
                    ident: Ident("id".into()),
                    type_: Type::Integer,
                },
                Field {
                    ident: Ident("name".into()),
                    type_: Type::String,
                },
            ],
        })
    );
    assert_parsed!(
        type_(),
        "enum { \"OK\", \"NG\"}",
        Type::Enum(Enum {
            variants: vec![Variant("OK".into()), Variant("NG".into())],
        })
    );

    assert_parsed!(type_(), "integer?", Type::Option(Box::new(Type::Integer)));
    assert_parsed!(
        type_(),
        "[integer?]?",
        Type::Option(Box::new(
            Type::Array(Box::new(Type::Option(Box::new(Type::Integer)))),
        ))
    );
}

#[test]
fn test_struct() {
    assert_parsed!(
        struct_(),
        "struct {id: integer, name: string}",
        Struct {
            title: None,
            fields: vec![
                Field {
                    ident: Ident("id".into()),
                    type_: Type::Integer,
                },
                Field {
                    ident: Ident("name".into()),
                    type_: Type::String,
                },
            ],
        }
    );

    assert_parsed!(
        struct_(),
        "struct \"User\" {id: integer, name: string}",
        Struct {
            title: Some("User".into()),
            fields: vec![
                Field {
                    ident: Ident("id".into()),
                    type_: Type::Integer,
                },
                Field {
                    ident: Ident("name".into()),
                    type_: Type::String,
                },
            ],
        }
    );

    assert_parse_fail!(struct_(), "struct {}");
    assert_parse_fail!(struct_(), "struct \"User\" {}");
}

#[test]
fn test_enum() {
    assert_parsed!(
        enum_(),
        "enum { \"OK\", \"NG\"}",
        Enum { variants: vec![Variant("OK".into()), Variant("NG".into())] }
    );

    assert_parse_fail!(enum_(), "enum {}");
}

#[test]
fn test_typedef() {
    assert_parsed!(
        typedef(),
        "type id = integer;",
        TypeDef {
            ident: Ident("id".into()),
            type_: Type::Integer,
        }
    );

    assert_parsed!(
        typedef(),
        "type user = struct \"User\" {id: id, name: string};",
        TypeDef {
            ident: Ident("user".into()),
            type_: Type::Struct(Struct {
                title: Some("User".into()),
                fields: vec![
                    Field {
                        ident: Ident("id".into()),
                        type_: Type::Ident(Ident("id".into())),
                    },
                    Field {
                        ident: Ident("name".into()),
                        type_: Type::String,
                    },
                ],
            }),
        }
    );
}

#[test]
fn test_blank() {
    assert_parsed!(
        blank(),
        "  

",
        ()
    );

    assert_parsed!(
        blank(),
        "
// this is comment
",
        ()
    );
}

#[test]
fn test_ast() {
    assert_parsed!(
        ast(),
        r#"
type id = integer;
type user = struct "User" {id: id, name: string};
"#,
        AST(vec![
            Item::TypeDef(TypeDef {
                ident: Ident("id".into()),
                type_: Type::Integer,
            }),
            Item::TypeDef(TypeDef {
                ident: Ident("user".into()),
                type_: Type::Struct(Struct {
                    title: Some("User".into()),
                    fields: vec![
                        Field {
                            ident: Ident("id".into()),
                            type_: Type::Ident(Ident("id".into())),
                        },
                        Field {
                            ident: Ident("name".into()),
                            type_: Type::String,
                        },
                    ],
                }),
            }),
        ])
    );

}
