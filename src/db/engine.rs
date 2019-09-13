use crate::abstract_model::{AbstractModel, AbstractTable, AbstractTableConstraintTypes};
use std::collections::{VecDeque, HashMap, HashSet};
use rstring_builder::StringBuilder;

pub trait DBEngineGenerator {
    fn generate_prefix(&self, sb: &mut StringBuilder, model: &AbstractModel);
    fn generate_table(&self, sb: &mut StringBuilder, table: &AbstractTable, model: &AbstractModel);
}

pub fn sort_tables(model: &AbstractModel) -> VecDeque<&AbstractTable> {
    let mut tables: VecDeque<&AbstractTable> = VecDeque::new();
    let mut remaining_tables: HashMap<&String, &AbstractTable> = HashMap::new();
    let mut available_tables: HashSet<&String> = HashSet::new();

    // add all tables into the remainingTables hashmap
    for table in &model.tables {
        remaining_tables.insert(&table.name, &table);
    }

    while !&remaining_tables.is_empty() {
        for (name, table) in &remaining_tables {
            let mut pickable = false;
            if table.constraints.is_empty() {
                // pick this table
                pickable = true;
            }
            else {
                pickable = true;
                for constraint in &table.constraints {
                    match &constraint._type {
                        AbstractTableConstraintTypes::FOREIGN_KEY {
                            source_columns,
                            reference,
                            reference_columns,
                        } => {
                            if !available_tables.contains(&reference) {
                                // pick this table
                                pickable = false;
                                break;
                            }
                        },
                        _ => {
                            // Ignore other constraints
                            continue
                        },
                    }
                }
            }

            // TODO: add check for column constraints

            if pickable {
                tables.push_back(table);
                available_tables.insert(name);
                continue;
            }
        }

        for name in &available_tables {
            remaining_tables.remove(name);
        }
    }

    tables
}

pub fn generate_ddl(model: &AbstractModel, engine: &DBEngineGenerator) {
    let tables = sort_tables(model);

    let mut builder = StringBuilder::new();
    engine.generate_prefix(&mut builder, model);

    for table in &tables {
        engine.generate_table(&mut builder, table, model);
    }

    println!("{}", builder.to_string());
}

