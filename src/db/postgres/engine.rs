use handlebars::{Handlebars, JsonRender, RenderContext, RenderError, Helper, Context, Output};
use crate::{
    db::engine::DBEngineGenerator,
    abstract_model::{AbstractModel, AbstractTable, AbstractTableColumnConstraintTypes, AbstractTableConstraintTypes, AbstractTableConstraints, AbstractColumn, AbstractDataType, UuidDefaults},
    db::postgres::types::TYPE_MAP
};
use rstring_builder::StringBuilder;

pub struct PostgresEngine {
    registry: handlebars::Handlebars
}

impl PostgresEngine {
    pub fn new() -> Self {
        let mut engine  = PostgresEngine {
            registry: Handlebars::new()
        };

        engine.registry.register_partial("table", include_str!("../../templates/postgres/table.sql"));
        engine.registry.register_partial("column", include_str!("../../templates/postgres/column.sql"));
        engine.registry.register_template_string("model", include_str!("../../templates/postgres/model.sql"));

        engine.registry.register_helper("column_type", Box::new(column_type_helper));

        return engine;
    }

    fn generate_table_constraint(&self, sb: &mut StringBuilder, constraint: &AbstractTableConstraints,
                            table: &AbstractTable, model: &AbstractModel) {
        match &constraint._type {
            AbstractTableConstraintTypes::PRIMARY_KEY {
                fields
            } => {
                sb.append(format!("PRIMARY KEY ({})", fields.join(", ")));
            },
            AbstractTableConstraintTypes::FOREIGN_KEY {
                source_columns,
                reference,
                reference_columns
            } => {
                sb.append(format!("FOREIGN KEY ({}) REFERENCES {} ({})",
                                    source_columns.join(", "),
                                    reference,
                                    reference_columns.join(", "))
                );
            }
        };
    }

    fn generate_table_field(&self, sb: &mut StringBuilder, field: &AbstractColumn,
                            table: &AbstractTable, model: &AbstractModel) {
        sb.append(format!("{}", field.name));

        // Type generation
        match &field._type {
            AbstractDataType::SERIAL => {
                sb.append(" SERIAL");
            },
            AbstractDataType::TEXT => {
                sb.append(" TEXT");
            },
            AbstractDataType::DECIMAL => {
                sb.append(" SERIAL");
            },
            AbstractDataType::UUID {
                default
            } => {
                sb.append(" UUID");
                match default {
                    Some(UuidDefaults::V4_RANDOM) => {
                        sb.append(" DEFAULT gen_random_uuid()");
                    },
                    _ => {}
                };
            },
            AbstractDataType::BIG_INTEGER {
                default
            } => {
                sb.append(" BIGINT");
                match default {
                    Some(v) => {
                        sb.append(format!(" DEFAULT {}", v));
                    },
                    _ => {}
                };
            },
            AbstractDataType::INTEGER {
                default
            } => {
                sb.append(" INTEGER");
                match default {
                    Some(v) => {
                        sb.append(format!(" DEFAULT {}", v));
                    },
                    _ => {}
                };
            },
            AbstractDataType::VARCHAR {
                length
            } => {
                sb.append(format!(" VARCHAR({})", length));
            },
        };

        // Not Null generation
        if field.not_null {
            sb.append(" NOT NULL");
        }

        // Constraints generation
        match &field.constraints {
            Some(constraints) => {
                for constraint in constraints {
                    match constraint {
                        AbstractTableColumnConstraintTypes::PRIMARY_KEY => {
                            sb.append(" PRIMARY KEY");
                        },
                        AbstractTableColumnConstraintTypes::FOREIGN_KEY {
                            reference,
                            reference_column
                        } => {
                            sb.append(format!(" FOREIGN KEY {} ({})", reference, reference_column));
                        },
                        AbstractTableColumnConstraintTypes::UNIQUE_KEY => {
                            sb.append(" UNIQUE");
                        }
                    }
                }
            },
            None => {}
        };
    }
}

impl DBEngineGenerator for PostgresEngine {
    fn generate_prefix(&self, sb: &mut StringBuilder, model: &AbstractModel) {
        sb.append("#\n");
        sb.append(format!("# POSTGRES DB SCHEMA for {}\n", model.name));
        sb.append("#\n\n");
    }

    fn generate_table(&self,
                      sb: &mut StringBuilder,
                      table: &AbstractTable,
                      model: &AbstractModel
    ) {
        sb.append("#\n");
        sb.append(format!("# Table: {} ({})\n", table.name, table.sql_name));
        sb.append("#\n");
        sb.append(format!("CREATE TABLE {} {{\n", table.sql_name));

        for field in &table.fields {
            sb.append("\t");
            self.generate_table_field(sb, field, table, model);
            sb.append(",\n");
        }

        if table.constraints.is_empty() {
            sb.delete_at(sb.len() - 2);
        }

        for constraint in &table.constraints {
            sb.append("\t");
            self.generate_table_constraint(sb, constraint, table, model);
            sb.append(",\n");
        }

        sb.delete_at(sb.len() - 2);
        sb.append("};\n\n");
    }
}

pub fn column_type_helper(h: &Helper,
                          _: &Handlebars,
                          c: &Context,
                          rc: &mut RenderContext,
                          out: &mut Output)
                          -> Result<(), RenderError> {
    let type_var =
        h.param(0)
            .ok_or_else(|| RenderError::new("Param not found for helper \"type\""))?;
    let type_value = type_var.value();
    let type_idx: String;
    if type_value.is_string() {
        // match type to postgres specific type
        type_idx = type_value.render().into();
    } else if type_value.is_object() {
        // match parameterized type to postgres specific type
        type_idx = type_value.as_object().unwrap().get("type").unwrap().render().into();
    } else {
        panic!("Unkown type type");
    }
    let type_format = match TYPE_MAP.get(&type_idx[..]) {
        Some(f) => f(type_value),
        None => panic!("Missing type implementation for PG for {}", type_idx)
    };
    out.write(&format!("{}", type_format))?;
    Ok(())
}

