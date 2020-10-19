use serde_json::{self, Map, Value};
use serde_yaml;
use crate::{Config, Format};

use std::io::stdout;

pub fn format(config: &Config, schema: Map<String, Value>) {
    let out = stdout();

    match config.format {
        Format::Yaml => serde_yaml::to_writer(out, &schema).expect("write yaml to the output"),
        Format::Json => {
            let res = if config.pack {
                serde_json::to_writer(out, &schema)
            } else {
                serde_json::to_writer_pretty(out, &schema)
            };
            res.expect("write json to the output")
        }
    };
}
