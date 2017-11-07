//! TOP = ITEMS
//! ITEMS = ITEM+
//! ITEM = TYPEDEF
//!
//! TYPEDEF = "type" IDENT "=" TYPE
//!
//! STRUCT = "struct" STRING? "{" (FIELD ",")+ "}"
//! FIELD = IDENT ":" TYPE
//!
//! ENUM = "enum" STRING? "{" (VARIANT",")+ "}"
//! VARIANT = STRING
//!
//! TYPE = "null" | "boolean" | "object" | "number" | "string" | "integer"
//!      | IDENT | "[" TYPE "]" | STRUCT | ENUM | TYPE "?"
//!      | TYPE "&" TYPE |  TYPE "|" TYPE | "(" TYPE ")"
//!
//! IDENT = [a-zA-Z_][a-zA-Z0-9_]*
//! STRIING = "\"" ([^"\\]|\.)* "\""
//!



use combine::{Parser, ParseError, Stream, State};
use combine::{skip_many, satisfy, optional, sep_by1, sep_end_by1, try, between, any, many};
use combine::char::{char, string, spaces};
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
    pub title: Option<String>,
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
    And(Vec<Type>),
    Or(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(pub String);


pub fn parse(input: &str) -> Result<AST, ParseError<State<&str>>> {
    ast().parse(State::new(input)).map(|r| r.0)
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
                _: char(';').message("typedef must end with ';'"),
            }
        }
    }
}

parser!{
    fn struct_[I]()(I) -> Struct
        where [I: Stream<Item=char>]
    {
        let field = struct_parser! {
            Field {
                ident: ident().skip(blank()),
                _: char(':').skip(blank()),
                type_: type_()
            }
        };

        struct_parser! {
            Struct {
                _: string("struct").skip(blank()),
                title: optional(str().skip(blank())),
                fields: between(char('{').skip(blank()),
                                char('}'),
                                sep_end_by1(field.skip(blank()), char(',').skip(blank()))),
            }
        }
    }
}


parser!{
    fn enum_[I]()(I) -> Enum
        where [I: Stream<Item=char>]
    {
        let variant = str()
            .message("enum variants must be strings")
            .map(Variant);

        struct_parser! {
            Enum {
                _: string("enum").skip(blank()),
                title: optional(str().skip(blank())),
                variants: between(char('{').skip(blank()),
                                  char('}'),
                                  sep_end_by1(variant.skip(blank()), char(',').skip(blank())))
            }
        }
    }
}

parser!{
    fn type0[I]()(I) -> Type
        where [I: Stream<Item=char>]
    {
        choice!(
            try(string("null").map(|_| Type::Null)),
            try(string("boolean").map(|_| Type::Boolean)),
            try(string("object").map(|_| Type::Object)),
            try(string("number").map(|_| Type::Number)),
            try(string("string").map(|_| Type::String)),
            try(string("integer").map(|_| Type::Integer)),
            try((char('[').skip(blank()), type_(), blank().with(char(']')))
                .map(|(_, ty, _)| Type::Array(Box::new(ty)))),
            try((char('(').skip(blank()), type_(), blank().with(char(')')))
                .map(|(_, ty, _)| ty)),
            try(struct_().map(Type::Struct)),
            try(enum_().map(Type::Enum)),
            ident().map(Type::Ident)
        )
    }
}

parser!{
    fn type1[I]()(I) -> Type
        where [I: Stream<Item=char>]
    {

        choice!(
            try(type0().skip(char('?')).map(|ty| Type::Option(Box::new(ty)))),
            type0()
        )
    }
}

parser! {
    fn type2[I]()(I) -> Type
        where [I: Stream<Item=char>]
    {
        let shift = |item: Type,  mut vec: Vec<Type>|  {
            vec.insert(0, item);
            vec
        };
        choice!(
            try((type1().skip(blank()).skip(string("&").skip(blank())), sep_by1(type1().skip(blank())
                            , string("&").skip(blank())
            ))
                .map(|(ty, tys)| Type::And(shift(ty, tys)))),
            try((type1().skip(blank()).skip(string("|").skip(blank())), sep_by1(type1().skip(blank())
                            , string("|").skip(blank())
            ))
                .map(|(ty, tys)| Type::Or (shift(ty, tys)))),
            type1()
        )
    }
}

parser!{
    fn type_[I]()(I) -> Type
        where [I: Stream<Item=char>]
    {
        type2()
    }
}

parser!{
    fn ident[I]()(I) -> Ident
        where [I: Stream<Item=char>]
    {
        recognize(satisfy(|c:char| c.is_alphabetic() || "_".contains(c))
                  .with(skip_many(satisfy(|c:char| c.is_alphanumeric() || "_".contains(c)))))
            .message("ident")
            .map(|s: String| Ident(s))
    }
}

parser!{
    fn str[I]()(I) -> String
        where [I: Stream<Item=char>]
    {
        between(char('"'), char('"'),
                many(char('\\').with(any()).or(satisfy(|c:char| c != '"'))))
            .message("string literal")
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



#[cfg(test)]
mod test {
    use combine::State;
    use super::*;

    macro_rules! assert_parsed {
        ($parser: expr, $input: expr, $expected: expr) => {
            assert_eq!($parser.parse(State::new($input)).map(|t| t.0), Ok($expected))
        }
    }

    macro_rules! assert_parse_fail {
        ($parser: expr, $input: expr) => {
            assert!($parser.parse($input).is_err())
        }
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
    fn test_ident() {
        assert_parsed!(ident(), "ident1", Ident("ident1".into()));

        assert_parsed!(ident(), "ident1  ", Ident("ident1".into()));

        assert_parsed!(ident(), "_ident1_  ", Ident("_ident1_".into()));

        assert_parse_fail!(ident(), "0ident1");
    }

    #[test]
    fn test_str() {
        assert_parsed!(str(), r#""""#, "".into());
        assert_parsed!(str(), r#""abc""#, "abc".into());
        assert_parsed!(str(), r#""abc\"def\"\\""#, "abc\"def\"\\".into());
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
            "struct {id: integer, name: string,}",
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
            Enum {
                title: None,
                variants: vec![Variant("OK".into()), Variant("NG".into())],
            }
        );

        assert_parsed!(
            enum_(),
            "enum { \"OK\", \"NG\",}",
            Enum {
                title: None,
                variants: vec![Variant("OK".into()), Variant("NG".into())],
            }
        );

        assert_parsed!(
            enum_(),
            "enum \"Result\" { \"OK\", \"NG\"}",
            Enum {
                title: Some("Result".to_string()),
                variants: vec![Variant("OK".into()), Variant("NG".into())],
            }
        );

        assert_parse_fail!(enum_(), "enum {}");
        assert_parse_fail!(enum_(), "enum \"Result\" {}");
    }

    #[test]
    fn test_type_null() {
        assert_parsed!(type_(), "null", Type::Null);
    }

    #[test]
    fn test_type_boolean() {
        assert_parsed!(type_(), "boolean", Type::Boolean);
    }

    #[test]
    fn test_type_object() {
        assert_parsed!(type_(), "object", Type::Object);
    }

    #[test]
    fn test_type_number() {
        assert_parsed!(type_(), "number", Type::Number);

    }

    #[test]
    fn test_type_string() {
        assert_parsed!(type_(), "string", Type::String);
    }

    #[test]
    fn test_type_integer() {
        assert_parsed!(type_(), "integer", Type::Integer);
    }

    #[test]
    fn test_type_ident() {
        assert_parsed!(type_(), "user", Type::Ident(Ident("user".into())));
    }

    #[test]
    fn test_type_array() {
        assert_parsed!(type_(), "[string]", Type::Array(Box::new(Type::String)));
    }

    #[test]
    fn test_type_struct() {
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
    }

    #[test]
    fn test_type_enum() {
        assert_parsed!(
            type_(),
            "enum { \"OK\", \"NG\"}",
            Type::Enum(Enum {
                title: None,
                variants: vec![Variant("OK".into()), Variant("NG".into())],
            })
        );
    }

    #[test]
    fn test_type_optional() {
        assert_parsed!(type_(), "integer?", Type::Option(Box::new(Type::Integer)));
    }

    #[test]
    fn test_type_or() {
        assert_parsed!(
            type_(),
            "integer | string",
            Type::Or(vec![Type::Integer, Type::String])
        );
    }

    #[test]
    fn test_type_and() {
        assert_parsed!(
            type_(),
            "struct {id: integer} & struct {name: string}",
            Type::And(vec![
                Type::Struct(Struct {
                    title: None,
                    fields: vec![
                        Field {
                            ident: Ident("id".into()),
                            type_: Type::Integer,
                        },
                    ],
                }),
                Type::Struct(Struct {
                    title: None,
                    fields: vec![
                        Field {
                            ident: Ident("name".into()),
                            type_: Type::String,
                        },
                    ],
                }),
            ])
        );
    }

    #[test]
    fn test_type_paren() {
        assert_parsed!(type_(), "(integer)", Type::Integer);
    }

    #[test]
    fn test_type() {
        assert_parsed!(
            type_(),
            "[[string]]",
            Type::Array(Box::new(Type::Array(Box::new(Type::String))))
        );

        assert_parsed!(
            type_(),
            "[integer?]?",
            Type::Option(Box::new(
                Type::Array(Box::new(Type::Option(Box::new(Type::Integer)))),
            ))
        );

        assert_parsed!(
            type_(),
            "(integer | string | null) & sometype",
            Type::And(vec![
                Type::Or(vec![Type::Integer, Type::String, Type::Null]),
                Type::Ident(Ident("sometype".into())),
            ])
        );
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

        assert_parsed!(
            typedef(),
            r#"type person = struct "Person" {
  id : struct { value: integer }
};"#,
            TypeDef {
                ident: Ident("person".into()),
                type_: Type::Struct(Struct {
                    title: Some("Person".into()),
                    fields: vec![
                        Field {
                            ident: Ident("id".into()),
                            type_: Type::Struct(Struct {
                                title: None,
                                fields: vec![
                                    Field {
                                        ident: Ident("value".into()),
                                        type_: Type::Integer,
                                    },
                                ],
                            }),
                        },
                    ],
                }),
            }
        );

        assert_parsed!(
            typedef(),
            r#"type person = struct "Person" {
  name: string,
  sex: enum { "M", "F" }
};"#,
            TypeDef {
                ident: Ident("person".into()),
                type_: Type::Struct(Struct {
                    title: Some("Person".into()),
                    fields: vec![
                        Field {
                            ident: Ident("name".into()),
                            type_: Type::String,
                        },
                        Field {
                            ident: Ident("sex".into()),
                            type_: Type::Enum(Enum {
                                title: None,
                                variants: vec![Variant("M".into()), Variant("F".into())],
                            }),
                        },
                    ],
                }),
            }
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
}
