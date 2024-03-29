//! TOP = ITEMS
//! ITEMS = ITEM+
//! ITEM = TYPEDEF
//!
//! TYPEDEF = "type" IDENT ""=" TYPE ";"
//!
//! TYPE = "null" | "boolean" | "object" | "number" | "string" | "integer"
//!      | IDENT | "[" TYPE "]" | STRUCT | ENUM | TYPE "?"
//!      | "format" "(" STRING ")" | "url" "(" STRING ")"
//!      | TYPE "&" TYPE |  TYPE "|" TYPE
//!      | TYPE "where" PRED
//!      | "(" TYPE ")" | STRING
//!
//! STRUCT = "struct" "{" (FIELD ",")+ "}"
//! FIELD = IDENT "?"? ":" TYPE
//!
//! ENUM = "enum" "{" (VARIANT",")+ "}"
//! VARIANT = STRING
//!
//! PRED = UNUMBER "<=" "length" | "length" <= UNUMBER
//!      | UNUMBER "<" "it" | "it" < UNUMBER
//!      | UNUMBER "<=" "it" | "it" <= UNUMBER
//!      | "it" "=" UNUMBER "*" "n"
//!      | "format" "=" STRING | "it" "=~" REGEX
//!      | PRED && PRED
//!
//! IDENT = [a-zA-Z_][a-zA-Z0-9_]*
//! STRIING = "\"" ([^"\\]|\\.)* "\""
//! REGEX   = "/" ([^/\\]|\\.)* "/"
//! UNUMBER = [0-9]+
//!
//! COMMENT = "//" any "\n" | "/*" any "*/"
//! DOC_COMMENT = "/**" any "*/"

use crate::Config;

use combine::parser::char::{char, digit, newline, spaces, string};
use combine::parser::combinator::{from_str, recognize};
use combine::stream::position::{SourcePosition, Stream as PositionedInput};
use combine::{
    any, attempt, between, many, many1, not_followed_by, optional, satisfy, sep_by1, sep_end_by1,
    skip_many,
};
use combine::{easy, EasyParser, ParseError, Parser, Stream};
use lazy_static::lazy_static;
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
    #[allow(dead_code)]
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
    pub is_optional: bool,
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
    MinSize(usize),
    MaxSize(usize),
    ExclusiveMinSize(usize),
    ExclusiveMaxSize(usize),
    MultipleOf(usize),
    Format(String),
    Match(String),
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
) -> Result<AST, easy::ParseError<PositionedInput<&'a str, SourcePosition>>> {
    let input = PositionedInput::new(input);
    ast().easy_parse(input).map(|r| r.0)
}

fn ast<'a, I>() -> impl Parser<I, Output = AST> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    blank().with(sep_end_by1(item(), blank())).map(AST)
}

fn item<'a, I>() -> impl Parser<I, Output = Item> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    typedef().map(Item::TypeDef)
}

fn typedef<'a, I>() -> impl Parser<I, Output = Annot<TypeDef>> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
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

fn struct_<'a, I>() -> impl Parser<I, Output = Annot<Struct>> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    let field = struct_parser! {
        Field {
            ident: ident().skip(blank()),
            is_optional: optional(char('?')).map(|o| o.is_some()),
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

fn enum_<'a, I>() -> impl Parser<I, Output = Annot<Enum>> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
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

fn type0<'a, I>() -> impl Parser<I, Output = Type> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    choice!(
        attempt(string("null").map(|_| Type::Null)),
        attempt(string("boolean").map(|_| Type::Boolean)),
        attempt(string("object").map(|_| Type::Object)),
        attempt(string("number").map(|_| Type::Number)),
        attempt(string("string").map(|_| Type::String)),
        attempt(string("integer").map(|_| Type::Integer)),
        attempt(
            between(
                string("format")
                    .skip(blank())
                    .skip(string("("))
                    .skip(blank()),
                string(")"),
                str_()
            )
            .map(|f| Type::Where(Box::new(Type::String), vec![Pred::Format(f)]))
        ),
        attempt(
            between(
                string("ref").skip(blank()).skip(string("(")).skip(blank()),
                string(")"),
                str_()
            )
            .map(Type::Ref)
        ),
        attempt(str_().map(|s| Type::Const(Const::String(s)))),
        attempt(
            (char('[').skip(blank()), type_(), blank().with(char(']')))
                .map(|(_, ty, _)| Type::Array(Box::new(ty)))
        ),
        attempt((char('(').skip(blank()), type_(), blank().with(char(')'))).map(|(_, ty, _)| ty)),
        attempt(struct_().map(Type::Struct)),
        attempt(enum_().map(Type::Enum)),
        ident().map(Type::Ident)
    )
}

fn type1<'a, I>() -> impl Parser<I, Output = Type> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    (type0(), optional(char('?'))).map(|(ty, q)| {
        if q.is_some() {
            Type::Option(Box::new(ty))
        } else {
            ty
        }
    })
}

fn type2<'a, I>() -> impl Parser<I, Output = Type> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    (
        type1().skip(blank()),
        optional((string("where").skip(blank()), preds())),
    )
        .map(|(ty, preds)| match preds {
            Some((_, preds)) => Type::Where(Box::new(ty), preds),
            None => ty,
        })
}

fn type3<'a, I>() -> impl Parser<I, Output = Type> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    fn shift(item: Type, mut vec: Vec<Type>) -> Vec<Type> {
        vec.insert(0, item);
        vec
    }

    choice!(
        attempt(
            (
                type2().skip(blank()).skip(string("&").skip(blank())),
                sep_by1(type2().skip(blank()), string("&").skip(blank()))
            )
                .map(|(ty, tys)| Type::And(shift(ty, tys)))
        ),
        attempt(
            (
                type2().skip(blank()).skip(string("|").skip(blank())),
                sep_by1(type2().skip(blank()), string("|").skip(blank()))
            )
                .map(|(ty, tys)| Type::Or(shift(ty, tys)))
        ),
        type2()
    )
}

parser! {
    fn type_[I]()(I) -> Type
    where [I: Stream<Token = char>]
    {
        opaque!(type3())
    }

}

fn preds<'a, I>() -> impl Parser<I, Output = Vec<Pred>> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    sep_by1(pred().skip(blank()), attempt(string("&&").skip(blank())))
}

fn pred<'a, I>() -> impl Parser<I, Output = Pred> + 'a
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    choice!(
        attempt(
            (
                string("format"),
                blank().skip(string("=")).skip(blank()),
                str_()
            )
                .map(|(_, _, s)| Pred::Format(s.to_string()))
        ),
        attempt(
            (
                string("it"),
                blank().skip(string("=~")).skip(blank()),
                regex()
            )
                .map(|(_, _, s)| Pred::Match(s.to_string()))
        ),
        attempt(
            (
                string("length"),
                blank().skip(string("<=")).skip(blank()),
                number()
            )
                .map(|(_, _, n)| Pred::MaxLength(n))
        ),
        attempt(
            (
                number(),
                blank().skip(string("<=")).skip(blank()),
                string("length")
            )
                .map(|(n, _, _)| Pred::MinLength(n))
        ),
        attempt(
            (
                string("it"),
                blank().skip(string("<")).skip(blank()),
                number()
            )
                .map(|(_, _, n)| Pred::ExclusiveMaxSize(n))
        ),
        attempt(
            (
                number(),
                blank().skip(string("<")).skip(blank()),
                string("it")
            )
                .map(|(n, _, _)| Pred::ExclusiveMinSize(n))
        ),
        attempt(
            (
                string("it"),
                blank().skip(string("<=")).skip(blank()),
                number()
            )
                .map(|(_, _, n)| Pred::MaxSize(n))
        ),
        attempt(
            (
                number(),
                blank().skip(string("<=")).skip(blank()),
                string("it")
            )
                .map(|(n, _, _)| Pred::MinSize(n))
        ),
        attempt(
            (
                string("it"),
                blank().skip(string("=")).skip(blank()),
                number().skip(blank()),
                string("*").skip(blank()),
                string("n")
            )
                .map(|(_, _, n, _, _)| Pred::MultipleOf(n))
        )
    )
}

fn ident<'a, I>() -> impl Parser<I, Output = Ident>
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    recognize(
        satisfy(|c: char| c.is_alphabetic() || "_".contains(c)).with(skip_many(satisfy(
            |c: char| c.is_alphanumeric() || "_".contains(c),
        ))),
    )
    .message("ident")
    .map(|s: String| Ident(s))
}

fn regex<'a, I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    between(
        char('/'),
        char('/'),
        many(char('\\').with(any()).or(satisfy(|c: char| c != '/'))),
    )
    .message("regex literal")
}

fn str_<'a, I>() -> impl Parser<I, Output = String>
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    between(
        char('"'),
        char('"'),
        many(char('\\').with(any()).or(satisfy(|c: char| c != '"'))),
    )
    .message("string literal")
}

fn number<'a, I>() -> impl Parser<I, Output = usize>
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    from_str(many1::<String, I, _>(digit()))
}

fn blank<'a, I>() -> impl Parser<I, Output = ()>
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    spaces().skip(skip_many(attempt(comment().skip(spaces()))))
}

fn comment<'a, I>() -> impl Parser<I, Output = ()>
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    let slasla = between(string("//"), newline(), skip_many(satisfy(|c| c != '\n')))
        .map(|_| ())
        .message("comment");
    let slaaster = between(
        string("/*").skip(not_followed_by(char('*'))),
        string("*/"),
        skip_many(satisfy(|c| c != '*').or(attempt(char('*').skip(not_followed_by(char('/')))))),
    );
    attempt(slasla).or(slaaster)
}

fn with_annot<'a, P, I>(p: P) -> impl Parser<I, Output = Annot<P::Output>> + 'a
where
    I: Stream<Token = char> + 'a,
    P: Parser<I> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
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

fn attribute_line(s: &str) -> Line<'_> {
    lazy_static! {
        static ref RE: Regex = Regex::new("^\\s*@(\\w+)\\s+(.*)").unwrap();
    }
    match RE.captures(s) {
        None => Line::Plain(s),
        Some(caps) => Line::Attribute(caps.get(1).unwrap().as_str(), caps.get(2).unwrap().as_str()),
    }
}

// to avoid type loop, manually define the function and return boxed type
fn doc_comments<'a, I>() -> impl Parser<I, Output = BTreeMap<String, String>>
where
    I: Stream<Token = char> + 'a,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
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
            satisfy(|c| c != '*').or(attempt(char('*').skip(not_followed_by(char('/'))))),
        )),
    )
    .map(process_docs)
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
            ]
            .into_iter()
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
            ]
            .into_iter()
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
                        is_optional: false,
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),
                    Annot::new(Field {
                        is_optional: false,
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
                        is_optional: false,
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),
                    Annot::new(Field {
                        is_optional: false,
                        ident: Ident("name".into()),
                        type_: Type::String,
                    }),
                ],
            })
        );

        assert_parsed!(
            struct_(),
            "struct {id?: integer}",
            Annot::new(Struct {
                fields: vec![Annot::new(Field {
                    is_optional: true,
                    ident: Ident("id".into()),
                    type_: Type::Integer,
                }),],
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
                                is_optional: false,
                                ident: Ident("id".into()),
                                type_: Type::Integer,
                            },
                            meta: Metadata {
                                doc: Some("doc".into()),
                                title: None,
                            },
                        },
                        Annot::new(Field {
                            is_optional: false,
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
                            is_optional: false,
                            ident: Ident("id".into()),
                            type_: Type::Integer,
                        }),
                        Annot::new(Field {
                            is_optional: false,
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
            Type::Where(
                Box::new(Type::String),
                vec![Pred::Format("date-time".to_string())]
            )
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
                        is_optional: false,
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),
                    Annot::new(Field {
                        is_optional: false,
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
    fn test_type_where() {
        assert_parsed!(
            type_(),
            r#"string where format = "email" && 1 <= length &&length<=100 && it =~ /[a-z]+@[a-z.]+/"#,
            Type::Where(
                Box::new(Type::String),
                vec![
                    Pred::Format("email".into()),
                    Pred::MinLength(1),
                    Pred::MaxLength(100),
                    Pred::Match("[a-z]+@[a-z.]+".into())
                ]
            )
        );
    }

    #[test]
    fn test_type_where_number() {
        assert_parsed!(
            type_(),
            r#"integer where 1 <= it &&it<100 && it = 5* n"#,
            Type::Where(
                Box::new(Type::Integer),
                vec![
                    Pred::MinSize(1),
                    Pred::ExclusiveMaxSize(100),
                    Pred::MultipleOf(5),
                ]
            )
        );
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
                        is_optional: false,
                        ident: Ident("id".into()),
                        type_: Type::Integer,
                    }),],
                })),
                Type::Struct(Annot::new(Struct {
                    fields: vec![Annot::new(Field {
                        is_optional: false,
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
            r#"string? where 1 <= length & string where format="email""#,
            Type::And(vec![
                Type::Where(
                    Box::new(Type::Option(Box::new(Type::String))),
                    vec![Pred::MinLength(1)]
                ),
                Type::Where(Box::new(Type::String), vec![Pred::Format("email".into())])
            ])
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
                        is_optional: false,
                        ident: Ident("code".into()),
                        type_: Type::Const(Const::String("not_json".into())),
                    }),
                    Annot::new(Field {
                        is_optional: false,
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
                                is_optional: false,
                                ident: Ident("id".into()),
                                type_: Type::Ident(Ident("id".into())),
                            }),
                            Annot::new(Field {
                                is_optional: false,
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
                                    is_optional: false,
                                    ident: Ident("id".into()),
                                    type_: Type::Ident(Ident("id".into())),
                                },
                                meta: Metadata {
                                    doc: Some("This is id".into()),
                                    title: None,
                                },
                            },
                            Annot::new(Field {
                                is_optional: false,
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
                            is_optional: false,
                            ident: Ident("id".into()),
                            type_: Type::Struct(Annot::new(Struct {
                                fields: vec![Annot::new(Field {
                                    is_optional: false,
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
                                is_optional: false,
                                ident: Ident("name".into()),
                                type_: Type::String,
                            }),
                            Annot::new(Field {
                                is_optional: false,
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
                                    is_optional: false,
                                    ident: Ident("id".into()),
                                    type_: Type::Ident(Ident("id".into())),
                                }),
                                Annot::new(Field {
                                    is_optional: false,
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
