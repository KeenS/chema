//! TOP = ITEMS
//! ITEMS = ITEM+
//! ITEM = TYPEDEF
//!
//! TYPEDEF = "type" IDENT "=" TYPE ";"
//!
//! TYPE = "null" | "boolean" | "object" | "number" | "string" | "integer"
//!      | IDENT | "[" TYPE "]" | STRUCT | ENUM | TYPE "?"
//!      | "format" "(" STRING ")" | "url" "(" STRING ")"
//!      | TYPE "&" TYPE |  TYPE "|" TYPE
//!      | TYPE "where" PRED
//!      | "(" TYPE ")" | STRING
//!
//! STRUCT = "struct" "{" (FIELD ",")+ "}"
//! FIELD = IDENT ":" TYPE
//!
//! ENUM = "enum" "{" (VARIANT",")+ "}"
//! VARIANT = STRING
//!
//! PRED = UNUMBER "<=" "length" | "length" <= UNUMBER
//!      | "format" "=" STRING
//!      | PRED && PRED
//!
//! IDENT = [a-zA-Z_][a-zA-Z0-9_]*
//! STRIING = "\"" ([^"\\]|\.)* "\""
//! UNUMBER = [0-9]+
//!
//! COMMENT = "//" any "\n" | "/*" any "*/"
//! DOC_COMMENT = "/**" any "*/"

use Config;

use combine::char::{char, digit, newline, spaces, string};
use combine::combinator::{from_str, recognize, sep_by};
use combine::ParseError;
use combine::{
    any, between, many, many1, not_followed_by, optional, satisfy, sep_by1, sep_end_by1, skip_many,
    try,
};
use combine::{easy, Parser, Stream};
use regex::Regex;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Metadata {
    pub title: Option<String>,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Annot<T> {
    pub t: T,
    pub meta: Metadata,
}

impl<T> Annot<T> {
    fn new(t: T) -> Self {
        Self {
            t,
            meta: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AST(pub Vec<Item>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Item {
    TypeDef(Annot<TypeDef>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDef {
    pub ident: Ident,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub fields: Vec<Annot<Field>>,
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
    Format(String),
    Ref(String),
    Ident(Ident),
    Const(Const),
    Array(Box<Type>),
    Struct(Annot<Struct>),
    Enum(Annot<Enum>),
    Option(Box<Type>),
    And(Vec<Type>),
    Or(Vec<Type>),
    Where(Box<Type>, Vec<Pred>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pred {
    MinLength(usize),
    MaxLength(usize),
    Format(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Const {
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(pub String);

pub fn parse<'cfg, 'a>(
    _: &'cfg Config,
    input: &'a str,
) -> Result<AST, easy::Errors<char, &'a str, usize>> {
    ast()
        .easy_parse(input)
        .map(|r| r.0)
        .map_err(|err| err.map_position(|p| p.translate_position(input)))
}

fn ast<'a, I>() -> impl Parser<Input = I, Output = AST> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    blank().with(sep_end_by1(item(), blank())).map(AST)
}

fn item<'a, I>() -> impl Parser<Input = I, Output = Item> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    typedef().map(Item::TypeDef)
}

fn typedef<'a, I>() -> impl Parser<Input = I, Output = Annot<TypeDef>> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let typedef_ = struct_parser! {
        TypeDef {
            _: string("type").skip(blank()),
            ident: ident().skip(blank()),
            _: char('=').skip(blank()),
            type_: type_().skip(blank()),
            _: char(';').message("typedef must end with ';'"),
        }
    };
    with_annot(typedef_)
}

fn struct_<'a, I>() -> impl Parser<Input = I, Output = Annot<Struct>> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let field = struct_parser! {
        Field {
            ident: ident().skip(blank()),
            _: char(':').skip(blank()),
            type_: type_()
        }
    };
    let field = with_annot(field);

    let struct__ = struct_parser! {
        Struct {
            _: string("struct").skip(blank()),
            fields: between(char('{').skip(blank()),
                            char('}'),
                            sep_end_by1(field.skip(blank()), char(',').skip(blank()))),
        }
    };
    with_annot(struct__)
}

fn enum_<'a, I>() -> impl Parser<Input = I, Output = Annot<Enum>> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let variant = str_().message("enum variants must be strings").map(Variant);

    let enum__ = struct_parser! {
        Enum {
            _: string("enum").skip(blank()),
            variants: between(char('{').skip(blank()),
                              char('}'),
                              sep_end_by1(variant.skip(blank()), char(',').skip(blank())))
        }
    };
    with_annot(enum__)
}

fn type0<'a, I>() -> impl Parser<Input = I, Output = Type> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        try(string("null").map(|_| Type::Null)),
        try(string("boolean").map(|_| Type::Boolean)),
        try(string("object").map(|_| Type::Object)),
        try(string("number").map(|_| Type::Number)),
        try(string("string").map(|_| Type::String)),
        try(string("integer").map(|_| Type::Integer)),
        try(between(
            string("format")
                .skip(blank())
                .skip(string("("))
                .skip(blank()),
            string(")"),
            str_()
        ).map(Type::Format)),
        try(between(
            string("ref").skip(blank()).skip(string("(")).skip(blank()),
            string(")"),
            str_()
        ).map(Type::Ref)),
        try(str_().map(|s| Type::Const(Const::String(s)))),
        try((char('[').skip(blank()), type_(), blank().with(char(']')))
            .map(|(_, ty, _)| Type::Array(Box::new(ty)))),
        try((char('(').skip(blank()), type_(), blank().with(char(')'))).map(|(_, ty, _)| ty)),
        try(struct_().map(Type::Struct)),
        try(enum_().map(Type::Enum)),
        ident().map(Type::Ident)
    )
}

fn type1<'a, I>() -> impl Parser<Input = I, Output = Type> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        try(type0().skip(char('?')).map(|ty| Type::Option(Box::new(ty)))),
        try((
            type0(),
            blank().skip(string("where")).skip(blank()),
            preds()
        )
            .map(|(ty, _, preds)| Type::Where(Box::new(ty), preds))),
        type0()
    )
}

fn type2<'a, I>() -> impl Parser<Input = I, Output = Type> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    fn shift(item: Type, mut vec: Vec<Type>) -> Vec<Type> {
        vec.insert(0, item);
        vec
    }

    choice!(
        try((
            type1().skip(blank()).skip(string("&").skip(blank())),
            sep_by1(type1().skip(blank()), string("&").skip(blank()))
        )
            .map(|(ty, tys)| Type::And(shift(ty, tys)))),
        try((
            type1().skip(blank()).skip(string("|").skip(blank())),
            sep_by1(type1().skip(blank()), string("|").skip(blank()))
        )
            .map(|(ty, tys)| Type::Or(shift(ty, tys)))),
        type1()
    )
}

parser! {
    fn type_[I]()(I) -> Type
    where [I: Stream<Item = char>]
    {
        opaque!(type2())
    }

}

fn preds<'a, I>() -> impl Parser<Input = I, Output = Vec<Pred>> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    sep_by(pred().skip(blank()), string("&&").skip(blank()))
}

fn pred<'a, I>() -> impl Parser<Input = I, Output = Pred> + 'a
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(
        try((
            string("length"),
            blank().skip(string("<=")).skip(blank()),
            number()
        )
            .map(|(_, _, n)| Pred::MaxLength(n))),
        try((
            number(),
            blank().skip(string("<=")).skip(blank()),
            string("length")
        )
            .map(|(n, _, _)| Pred::MinLength(n)))
    )
}

fn ident<'a, I>() -> impl Parser<Input = I, Output = Ident>
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    recognize(
        satisfy(|c: char| c.is_alphabetic() || "_".contains(c)).with(skip_many(satisfy(
            |c: char| c.is_alphanumeric() || "_".contains(c),
        ))),
    ).message("ident")
    .map(|s: String| Ident(s))
}

fn str_<'a, I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        char('"'),
        char('"'),
        many(char('\\').with(any()).or(satisfy(|c: char| c != '"'))),
    ).message("string literal")
}

fn number<'a, I>() -> impl Parser<Input = I, Output = usize>
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    from_str(many1::<String, _>(digit()))
}

fn blank<'a, I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces().skip(skip_many(try(comment().skip(spaces()))))
}

fn comment<'a, I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let slasla = between(string("//"), newline(), skip_many(satisfy(|c| c != '\n')))
        .map(|_| ())
        .message("comment");
    let slaaster = between(
        string("/*").skip(not_followed_by(char('*'))),
        string("*/"),
        skip_many(satisfy(|c| c != '*').or(try(char('*').skip(not_followed_by(char('/')))))),
    );
    try(slasla).or(slaaster)
}

fn with_annot<'a, P, I>(p: P) -> impl Parser<Input = I, Output = Annot<P::Output>> + 'a
where
    I: Stream<Item = char> + 'a,
    P: Parser<Input = I> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    optional(doc_comments().skip(blank()))
        .and(p)
        .map(|(data, t)| {
            let meta = if let Some(mut data) = data {
                Metadata {
                    doc: data.remove("desc"),
                    title: data.remove("title"),
                }
            } else {
                Metadata {
                    doc: None,
                    title: None,
                }
            };
            Annot { t, meta }
        })
}

fn leading_aster(s: &str) -> &str {
    lazy_static! {
        static ref RE: Regex = Regex::new("^\\s*\\*").unwrap();
    }

    match RE.find(s) {
        None => s,
        Some(mtch) => &s[mtch.end()..],
    }
}

enum Line<'a> {
    Plain(&'a str),
    Attribute(&'a str, &'a str),
}

fn attribute_line(s: &str) -> Line {
    lazy_static! {
        static ref RE: Regex = Regex::new("^\\s*@(\\w+)\\s+(.*)").unwrap();
    }
    match RE.captures(s) {
        None => Line::Plain(s),
        Some(caps) => Line::Attribute(caps.get(1).unwrap().as_str(), caps.get(2).unwrap().as_str()),
    }
}

// to avoid type loop, manually define the function and return boxed type
fn doc_comments<'a, I>() -> impl Parser<Input = I, Output = BTreeMap<String, String>>
where
    I: Stream<Item = char> + 'a,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    fn process_docs(s: String) -> BTreeMap<String, String> {
        let mut desc = Vec::new();
        let mut attrs = Vec::new();
        let lines = s.trim().lines().map(leading_aster).map(attribute_line);
        for line in lines {
            match line {
                Line::Plain(s) => desc.push(s),
                Line::Attribute(k, v) => attrs.push((k.to_string(), v.to_string())),
            }
        }
        let desc = desc.join("\n").trim().to_string();
        if !desc.is_empty() {
            attrs.push(("desc".into(), desc));
        }
        attrs.into_iter().collect()
    }

    between(
        string("/**"),
        string("*/"),
        recognize(skip_many(
            satisfy(|c| c != '*').or(try(char('*').skip(not_followed_by(char('/'))))),
        )),
    ).map(process_docs)
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_parsed {
        ($parser: expr, $input: expr, $expected: expr) => {
            assert_parsed!($parser, $input, $expected, "")
        };
        ($parser: expr, $input: expr, $expected: expr, $rest: expr) => {
            assert_eq!(
                $parser.easy_parse($input).map(|(t1, t2)| (t1, t2)),
                Ok(($expected, $rest))
            )
        };
    }

    macro_rules! assert_parsed_partial {
        ($parser: expr, $input: expr, $expected: expr) => {
            assert_eq!($parser.easy_parse($input).map(|t| t.0), Ok($expected))
        };
    }

    macro_rules! assert_parse_fail {
        ($parser: expr, $input: expr) => {
            assert!($parser.parse($input).is_err())
        };
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
            r#"
// this is comment
// continued
"#,
            ()
        );

        {
            let input = "/// doc comments aren't blank";
            assert_parsed!(blank(), input, (), input);
        }

        assert_parsed!(blank(), "/* comment */", ());
        assert_parsed!(blank(), "/* comment * comment */", ());
        assert_parsed!(blank(), "/* comment * / comment */", ());
        assert_parsed!(blank(), "/* comment /**/", ());
    }

    #[test]
    fn test_doc_comments() {
        assert_parsed!(
            doc_comments(),
            r#"/**single line*/"#,
            vec![("desc".into(), "single line".into())]
                .into_iter()
                .collect()
        );

        assert_parsed_partial!(
            doc_comments(),
            r#"/** doc */ other data"#,
            vec![("desc".into(), "doc".into())].into_iter().collect()
        );

        assert_parsed!(
            doc_comments(),
            r#"/** multiple
line */"#,
            vec![("desc".into(), "multiple\nline".into())]
                .into_iter()
                .collect()
        );

        assert_parsed!(
            doc_comments(),
            r#"/** multiple
                * line
                */"#,
            vec![("desc".into(), "multiple\n line".into())]
                .into_iter()
                .collect()
        );

        assert_parsed!(
            doc_comments(),
            r#"/** multiple
                * line
                * @title Title
                */"#,
            vec![
                ("desc".into(), "multiple\n line".into()),
                ("title".into(), "Title".into()),
            ].into_iter()
            .collect()
        );

        assert_parsed!(
            doc_comments(),
            r#"/** multiple
                * line
                * @title Title
                * @attr   unknown attribute
                */"#,
            (vec![
                ("desc".into(), "multiple\n line".into()),
                ("title".into(), "Title".into()),
                ("attr".into(), "unknown attribute".into()),
            ].into_iter()
            .collect())
        );
    }

    #[test]
    fn test_ident() {
        assert_parsed!(ident(), "ident1", Ident("ident1".into()));

        assert_parsed!(ident(), "ident1  ", Ident("ident1".into()), "  ");

        assert_parsed!(ident(), "_ident1_  ", Ident("_ident1_".into()), "  ");

        assert_parse_fail!(ident(), "0ident1");
    }

    #[test]
    fn test_str_() {
        assert_parsed!(str_(), r#""""#, "".into());
        assert_parsed!(str_(), r#""abc""#, "abc".into());
        assert_parsed!(str_(), r#""abc\"def\"\\""#, "abc\"def\"\\".into());
    }

    #[test]
    fn test_struct() {
        assert_parsed!(
            struct_(),
            "struct {id: integer, name: string}",
            Annot::new(Struct {
                fields: vec![
                    Annot::new(Field {
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),
                    Annot::new(Field {
                        ident: Ident("name".into()),
                        type_: Type::String,
                    }),
                ],
            })
        );

        assert_parsed!(
            struct_(),
            "struct {id: integer, name: string,}",
            Annot::new(Struct {
                fields: vec![
                    Annot::new(Field {
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),
                    Annot::new(Field {
                        ident: Ident("name".into()),
                        type_: Type::String,
                    }),
                ],
            })
        );

        assert_parsed!(
            struct_(),
            "/** doc */
struct {
  /** doc */
  id: integer,
  name: string,
}",
            Annot {
                t: Struct {
                    fields: vec![
                        Annot {
                            t: Field {
                                ident: Ident("id".into()),
                                type_: Type::Integer,
                            },
                            meta: Metadata {
                                doc: Some("doc".into()),
                                title: None,
                            },
                        },
                        Annot::new(Field {
                            ident: Ident("name".into()),
                            type_: Type::String,
                        }),
                    ],
                },
                meta: Metadata {
                    doc: Some("doc".into()),
                    title: None,
                },
            }
        );

        assert_parsed!(
            struct_(),
            "/** @title User */struct {id: integer, name: string}",
            Annot {
                t: (Struct {
                    fields: vec![
                        Annot::new(Field {
                            ident: Ident("id".into()),
                            type_: Type::Integer,
                        }),
                        Annot::new(Field {
                            ident: Ident("name".into()),
                            type_: Type::String,
                        }),
                    ],
                }),
                meta: Metadata {
                    doc: None,
                    title: Some("User".into()),
                },
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
            Annot::new(Enum {
                variants: vec![Variant("OK".into()), Variant("NG".into())],
            })
        );

        assert_parsed!(
            enum_(),
            "enum { \"OK\", \"NG\",}",
            Annot::new(Enum {
                variants: vec![Variant("OK".into()), Variant("NG".into())],
            })
        );
        assert_parsed!(
            enum_(),
            "/** doc */
enum { \"OK\", \"NG\",}",
            Annot {
                t: (Enum {
                    variants: vec![Variant("OK".into()), Variant("NG".into())],
                }),
                meta: Metadata {
                    doc: Some("doc".into()),
                    title: None,
                },
            }
        );

        assert_parsed!(
            enum_(),
            "/** @title Result*/ enum { \"OK\", \"NG\"}",
            Annot {
                t: (Enum {
                    variants: vec![Variant("OK".into()), Variant("NG".into())],
                }),
                meta: Metadata {
                    doc: None,
                    title: Some("Result".to_string()),
                },
            }
        );

        assert_parse_fail!(enum_(), "enum {}");
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
    fn test_type_format() {
        assert_parsed!(
            type_(),
            r#"format("date-time")"#,
            Type::Format("date-time".to_string())
        );
    }

    #[test]
    fn test_type_ref() {
        assert_parsed!(
            type_(),
            r#"ref("http://json-schema.org/draft-07/hyper-schema")"#,
            Type::Ref("http://json-schema.org/draft-07/hyper-schema".to_string())
        );
    }

    #[test]
    fn test_type_ident() {
        assert_parsed!(type_(), "user", Type::Ident(Ident("user".into())));
    }

    #[test]
    fn test_type_const() {
        assert_parsed!(
            type_(),
            r#""const literal""#,
            Type::Const(Const::String("const literal".into()))
        );
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
            Type::Struct(Annot::new(Struct {
                fields: vec![
                    Annot::new(Field {
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),
                    Annot::new(Field {
                        ident: Ident("name".into()),
                        type_: Type::String,
                    }),
                ],
            }))
        );
    }

    #[test]
    fn test_type_enum() {
        assert_parsed!(
            type_(),
            "enum { \"OK\", \"NG\"}",
            Type::Enum(Annot::new(Enum {
                variants: vec![Variant("OK".into()), Variant("NG".into())],
            }))
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
                Type::Struct(Annot::new(Struct {
                    fields: vec![Annot::new(Field {
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),],
                })),
                Type::Struct(Annot::new(Struct {
                    fields: vec![Annot::new(Field {
                        ident: Ident("name".into()),
                        type_: Type::String,
                    }),],
                })),
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
            Type::Option(Box::new(Type::Array(Box::new(Type::Option(Box::new(
                Type::Integer
            )))),))
        );

        assert_parsed!(
            type_(),
            "(integer | string | null) & sometype",
            Type::And(vec![
                Type::Or(vec![Type::Integer, Type::String, Type::Null]),
                Type::Ident(Ident("sometype".into())),
            ])
        );

        assert_parsed!(
            type_(),
            r#"struct { code: "not_json", message: string}"#,
            Type::Struct(Annot::new(Struct {
                fields: vec![
                    Annot::new(Field {
                        ident: Ident("code".into()),
                        type_: Type::Const(Const::String("not_json".into())),
                    }),
                    Annot::new(Field {
                        ident: Ident("message".into()),
                        type_: Type::String,
                    }),
                ],
            }))
        );
    }

    #[test]
    fn test_typedef() {
        assert_parsed!(
            typedef(),
            "type id = integer;",
            Annot::new(TypeDef {
                ident: Ident("id".into()),
                type_: Type::Integer,
            })
        );

        assert_parsed!(
            typedef(),
            "/** @title User */ type user = struct {id: id, name: string};",
            Annot {
                t: TypeDef {
                    ident: Ident("user".into()),
                    type_: Type::Struct(Annot::new(Struct {
                        fields: vec![
                            Annot::new(Field {
                                ident: Ident("id".into()),
                                type_: Type::Ident(Ident("id".into())),
                            }),
                            Annot::new(Field {
                                ident: Ident("name".into()),
                                type_: Type::String,
                            }),
                        ],
                    })),
                },
                meta: Metadata {
                    doc: None,
                    title: Some("User".into()),
                },
            }
        );

        assert_parsed!(
            typedef(),
            r#"/** This is User
                 * @title User */
type user = struct {
  /** This is id */
  id: id,
  // comment is ignored
  name: string,
};"#,
            Annot {
                t: TypeDef {
                    ident: Ident("user".into()),
                    type_: Type::Struct(Annot::new(Struct {
                        fields: vec![
                            Annot {
                                t: Field {
                                    ident: Ident("id".into()),
                                    type_: Type::Ident(Ident("id".into())),
                                },
                                meta: Metadata {
                                    doc: Some("This is id".into()),
                                    title: None,
                                },
                            },
                            Annot::new(Field {
                                ident: Ident("name".into()),
                                type_: Type::String,
                            }),
                        ],
                    })),
                },
                meta: Metadata {
                    doc: Some("This is User".into()),
                    title: Some("User".into()),
                },
            }
        );

        assert_parsed!(
            typedef(),
            r#"/** @title Person */
type person = struct {
  id : struct { value: integer }
};"#,
            Annot {
                t: TypeDef {
                    ident: Ident("person".into()),
                    type_: Type::Struct(Annot::new(Struct {
                        fields: vec![Annot::new(Field {
                            ident: Ident("id".into()),
                            type_: Type::Struct(Annot::new(Struct {
                                fields: vec![Annot::new(Field {
                                    ident: Ident("value".into()),
                                    type_: Type::Integer,
                                }),],
                            })),
                        }),],
                    })),
                },
                meta: Metadata {
                    doc: None,
                    title: Some("Person".into()),
                },
            }
        );

        assert_parsed!(
            typedef(),
            r#"/** @title Person */
type person = struct {
  name: string,
  sex: enum { "M", "F" }
};"#,
            Annot {
                t: TypeDef {
                    ident: Ident("person".into()),
                    type_: Type::Struct(Annot::new(Struct {
                        fields: vec![
                            Annot::new(Field {
                                ident: Ident("name".into()),
                                type_: Type::String,
                            }),
                            Annot::new(Field {
                                ident: Ident("sex".into()),
                                type_: Type::Enum(Annot::new(Enum {
                                    variants: vec![Variant("M".into()), Variant("F".into())],
                                })),
                            }),
                        ],
                    })),
                },
                meta: Metadata {
                    doc: None,
                    title: Some("Person".into()),
                },
            }
        );
    }

    #[test]
    fn test_ast() {
        assert_parsed!(
            ast(),
            r#"
type id = integer;
/** @title User */
type user = struct {id: id, name: string};

"#,
            AST(vec![
                Item::TypeDef(Annot::new(TypeDef {
                    ident: Ident("id".into()),
                    type_: Type::Integer,
                })),
                Item::TypeDef(Annot {
                    t: (TypeDef {
                        ident: Ident("user".into()),
                        type_: Type::Struct(Annot::new(Struct {
                            fields: vec![
                                Annot::new(Field {
                                    ident: Ident("id".into()),
                                    type_: Type::Ident(Ident("id".into())),
                                }),
                                Annot::new(Field {
                                    ident: Ident("name".into()),
                                    type_: Type::String,
                                }),
                            ],
                        })),
                    }),
                    meta: Metadata {
                        doc: None,
                        title: Some("User".into()),
                    },
                }),
            ])
        );
    }
}
