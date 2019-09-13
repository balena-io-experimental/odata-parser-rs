use serde_json::Value;
use crate::abstract_model::{AbstractModel, AbstractTableConstraintTypes, AbstractDataType};

pub trait DBEngine {
    fn render_ddl(&self, model: &AbstractModel) -> Result<String, handlebars::RenderError>;
}

pub enum DBEngines {
    Postgres
}

//pub type type_renderer = fn(value: &AbstractDataType) -> String;

pub type type_renderer = fn(value: &Value) -> String;

