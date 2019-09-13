use serde_json::Value;
use std::collections::HashMap;
use crate::db::common::type_renderer;
use crate::abstract_model::{VARCHAR, INTEGER, BIG_INTEGER, DECIMAL, TEXT, UUID, SERIAL};

lazy_static! {
    pub static ref TYPE_MAP: HashMap<&'static str, type_renderer> = {
        hashmap!{
            INTEGER => POSTGRES_INTEGER_PTR,
            BIG_INTEGER => postgres_bigint,
            SERIAL => postgres_serial,
            DECIMAL => postgres_decimal,
            TEXT => postgres_text,
            UUID => postgres_uuid,
            VARCHAR => POSTGRES_VARCHAR_PTR,
        }
    };
}

fn postgres_integer(value: &Value) -> String {
    "INTEGER".into()
}

fn postgres_bigint(value: &Value) -> String {
    "BIGINT".into()
}

fn postgres_serial(value: &Value) -> String {
    "SERIAL".into()
}

fn postgres_decimal(value: &Value) -> String {
    "DECIMAL".into()
}

fn postgres_text(value: &Value) -> String {
    "TEXT".into()
}

fn postgres_uuid(value: &Value) -> String {
    match value.as_object().unwrap().get("default") {
        Some(function) => {
            match function.as_str() {
                Some("V4_RANDOM") => "UUID DEFAULT gen_random_uuid()".into(),
                Some(v) => panic!("Unkown UUID default for postgres {}", v),
                _ => panic!("Invalid value for uuid default")
            }
        },
        None => "UUID".into()
    }
}

fn postgres_varchar(value: &Value) -> String {
    format!("VARCHAR({})", value.as_object().unwrap().get("length").unwrap())
}

static POSTGRES_INTEGER_PTR: type_renderer = postgres_integer;

static POSTGRES_VARCHAR_PTR: type_renderer = postgres_varchar;
