use crate::schema::Document;
use serde::{Serialize};

#[derive(Serialize)]
pub struct AbstractModel {
    pub name: String,
    pub tables: Vec<AbstractTable>,
}

#[derive(Serialize)]
pub struct AbstractTable {
    pub name: String,
    pub sql_name: String,
    pub fields: Vec<AbstractColumn>,
    pub constraints: Vec<AbstractTableConstraints>,
}

#[derive(Serialize)]
pub struct AbstractTableConstraints {
    pub name: String,
    pub _type: AbstractTableConstraintTypes,
}

#[derive(Serialize)]
pub enum AbstractTableConstraintTypes {
    FOREIGN_KEY {
        source_columns: Vec<String>,
        reference: String,
        reference_columns: Vec<String>,
    },
    PRIMARY_KEY {
        fields: Vec<String>
    }
}

#[derive(Serialize)]
pub enum AbstractTableColumnConstraintTypes {
    FOREIGN_KEY {
        reference: String,
        reference_column: String,
    },
    PRIMARY_KEY,
    UNIQUE_KEY,
}

#[derive(Serialize)]
pub struct AbstractColumn {
    pub name: String,
    pub _type: AbstractDataType,
    pub not_null: bool,
    pub constraints: Option<Vec<AbstractTableColumnConstraintTypes>>
}

pub const INTEGER: &str = "INTEGER";
pub const BIG_INTEGER: &str = "BIG_INTEGER";
pub const SERIAL: &str = "SERIAL";
pub const DECIMAL: &str = "DECIMAL";
pub const VARCHAR: &str = "VARCHAR";
pub const TEXT: &str = "TEXT";
pub const UUID: &str = "UUID";

#[derive(Serialize)]
pub enum UuidDefaults {
    V4_RANDOM
}

#[derive(Serialize)]
#[serde(tag = "type")]
pub enum AbstractDataType {
    #[serde(rename = INTEGER)]
    INTEGER {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<u32>
    },
    #[serde(rename = BIG_INTEGER)]
    BIG_INTEGER {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<u64>
    },
    #[serde(rename = SERIAL)]
    SERIAL,
    #[serde(rename = DECIMAL)]
    DECIMAL,
    #[serde(rename = VARCHAR)]
    VARCHAR {length: u8},
    #[serde(rename = TEXT)]
    TEXT,
    #[serde(rename = UUID)]
    UUID {
        #[serde(skip_serializing_if = "Option::is_none")]
        default: Option<UuidDefaults>
    },
}
