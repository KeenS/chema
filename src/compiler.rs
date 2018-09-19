use parser::*;
use Config;

use serde_json::{Map, Value};

fn insert_if(mut map: Map<String, Value>, k: &str, v: Option<String>) -> Map<String, Value> {
    if let Some(v) = v {
        map.insert(k.into(), Value::String(v));
    }
    map
}

pub fn compile(config: &Config, ast: AST) -> Map<String, Value> {
    ast.0
        .into_iter()
        .map(|Item::TypeDef(td)| {
            let type_ = compile_type(config, td.t.type_);
            let type_ = insert_if(type_, "description", td.meta.doc);
            let type_ = insert_if(type_, "title", td.meta.title);
            (td.t.ident.0, Value::Object(type_))
        }).collect()
}

fn compile_type(config: &Config, ty: Type) -> Map<String, Value> {
    use self::Type::*;
    let kvs = match ty {
        Null => vec![("type".to_string(), Value::String("null".into()))],
        Boolean => vec![("type".to_string(), Value::String("boolean".into()))],
        Object => vec![("type".to_string(), Value::String("object".into()))],
        Number => vec![("type".to_string(), Value::String("number".into()))],
        String => vec![("type".to_string(), Value::String("string".into()))],
        Integer => vec![("type".to_string(), Value::String("integer".into()))],
        Format(s) => vec![
            ("type".to_string(), Value::String("string".into())),
            ("format".to_string(), Value::String(s)),
        ],
        Ref(url) => vec![("$ref".to_string(), Value::String(url))],
        Ident(::parser::Ident(i)) => vec![(
            "$ref".to_string(),
            Value::String(format!("#{}/{}", &config.path_prefix, i)),
        )],
        Const(c) => {
            use parser::Const::*;
            match c {
                String(s) => vec![("constant".to_string(), Value::String(s))],
            }
        }
        Array(ty) => vec![
            ("type".to_string(), Value::String("array".into())),
            (
                "items".to_string(),
                Value::Object(compile_type(config, *ty)),
            ),
        ],
        Struct(Annot {
            t: ::parser::Struct { fields },
            meta,
        }) => {
            let required = collect_requied(&fields)
                .into_iter()
                .map(|id| Value::String(id.0))
                .collect();
            let properties = fields
                .into_iter()
                .map(|f| {
                    (
                        f.t.ident.0,
                        Value::Object(insert_if(
                            compile_type(config, f.t.type_),
                            "description",
                            f.meta.doc,
                        )),
                    )
                }).collect();
            let mut vec = vec![
                ("type".to_string(), Value::String("object".to_string())),
                ("properties".to_string(), Value::Object(properties)),
                ("required".to_string(), Value::Array(required)),
            ];
            if let Some(title) = meta.title {
                vec.push(("title".to_string(), Value::String(title)));
            }
            if let Some(doc) = meta.doc {
                vec.push(("description".to_string(), Value::String(doc)))
            }

            vec
        }
        Enum(Annot {
            t: ::parser::Enum { variants },
            meta,
        }) => {
            let variants = variants.into_iter().map(|v| Value::String(v.0)).collect();
            let mut vec = vec![
                ("type".to_string(), Value::String("string".into())),
                ("enum".to_string(), Value::Array(variants)),
            ];
            if let Some(title) = meta.title {
                vec.push(("title".to_string(), Value::String(title)));
            }
            if let Some(doc) = meta.doc {
                vec.push(("description".to_string(), Value::String(doc)))
            }
            vec
        }
        Option(ty) => {
            let mut map = compile_type(config, *ty);
            if config.no_swagger {
                let null = compile_type(config, Type::Null);
                vec![("oneOf".into(), Value::Array(vec![map.into(), null.into()]))]
            } else {
                // Swagger only
                map.insert("nullable".into(), Value::Bool(true));
                map.into_iter().collect()
            }
        }
        And(tys) => {
            let tys = tys
                .into_iter()
                .map(|ty| compile_type(config, ty))
                .map(Value::Object)
                .collect::<Vec<_>>();
            vec![("allOf".into(), Value::Array(tys))]
        }
        Or(tys) => {
            let tys = tys
                .into_iter()
                .map(|ty| compile_type(config, ty))
                .map(Value::Object)
                .collect::<Vec<_>>();
            vec![("anyOf".into(), Value::Array(tys))]
        }
    };
    kvs.into_iter().collect()
}

fn collect_requied(fs: &Vec<Annot<Field>>) -> Vec<Ident> {
    fs.iter().filter_map(|f| Some(f.t.ident.clone())).collect()
}
