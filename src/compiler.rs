use parser::*;
use serde_json::{Map, Value};

pub fn compile(ast: AST) -> Map<String, Value> {
    ast.0
        .into_iter()
        .map(|Item::TypeDef(td)| {
            (td.ident.0, Value::Object(compile_type(td.type_)))
        })
        .collect()
}

fn compile_type(ty: Type) -> Map<String, Value> {
    use self::Type::*;
    let kvs = match ty {
        Null => vec![("type".to_string(), Value::String("null".into()))],
        Boolean => vec![("type".to_string(), Value::String("boolean".into()))],
        Object => vec![("type".to_string(), Value::String("object".into()))],
        Number => vec![("type".to_string(), Value::String("number".into()))],
        String => vec![("type".to_string(), Value::String("string".into()))],
        Integer => vec![("type".to_string(), Value::String("integer".into()))],
        Ident(::parser::Ident(i)) => {
            vec![
                (
                    "$ref".to_string(),
                    Value::String(format!("#/definition/{}", i))
                ),
            ]
        }
        Array(ty) => {
            vec![
                ("type".to_string(), Value::String("array".into())),
                ("items".to_string(), Value::Object(compile_type(*ty))),
            ]
        }
        Struct(::parser::Struct { title, fields }) => {
            let required = collect_requied(&fields)
                .into_iter()
                .map(|id| Value::String(id.0))
                .collect();
            let properties = fields
                .into_iter()
                .map(|f| (f.ident.0, Value::Object(compile_type(f.type_))))
                .collect();
            vec![
                ("type".to_string(), Value::String("object".to_string())),
                ("properties".to_string(), Value::Object(properties)),
                ("required".to_string(), Value::Array(required)),
            ]
        }
        Enum(::parser::Enum { variants }) => {
            let variants = variants.into_iter().map(|v| Value::String(v.0)).collect();
            vec![("enum".to_string(), Value::Array(variants))]
        }
        Option(ty) => {
            let mut map = compile_type(*ty);
            // Swagger only
            map.insert("nullable".into(), Value::Bool(true));
            map.into_iter().collect()
        }
    };
    kvs.into_iter().collect()
}

fn collect_requied(fs: &Vec<Field>) -> Vec<Ident> {
    fs.iter().filter_map(|f| Some(f.ident.clone())).collect()
}
