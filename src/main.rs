#![recursion_limit = "128"]
#![feature(const_fn)]

#[macro_use]
extern crate handlebars;
#[macro_use]
extern crate nom;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate lazy_static;

extern crate rstring_builder;

mod ast;
mod csdl;
mod derefcell;
mod parser;
mod schema;
mod util;
mod abstract_model;
mod db;


use derefcell::DerefCell;
use schema::map;
use schema::property::{Navigation, Structural};
use schema::ty::*;
use schema::{Document, EntityContainer, EntityContainerPath, EntitySet, Schema};
use db::engine::generate_ddl;
use crate::schema::Action;
use crate::abstract_model::{UuidDefaults, AbstractModel, AbstractTable, AbstractColumn, AbstractDataType, AbstractTableConstraints, AbstractTableColumnConstraintTypes, AbstractTableConstraintTypes};

fn main() {
    let namespace = "com.balena_cloud.api".to_string();
    let doc = Document {
        version: "4.01".into(),
        service_root: "http://example.com/foobar/".into(),
        entity_container: DerefCell::with_state(EntityContainerPath(
            namespace.clone(),
            "balena".to_string(),
        )),
        schemas: map(&[Schema {
            namespace: namespace.clone(),
            type_definitions: map( &[
                Definition {
                    name: "Weight".into(),
                    underlying_type: Primitive::Int32,
                    ..Definition::default()
                }
            ]),
            enum_types: map(&[
                Enum {
                    name: "device state".into(),
                    members: hashmap!{
                        "offline".into() => 0,
                        "online".into() => 1,
                    },
                    ..Enum::default()
                }
            ]),
            complex_types: map( &[
                Complex {
                    name: "coordinates".into(),
                    structural_props: map( &[
                        Structural {
                            name: "longtitude".into(),
                            ty: Ty::Primitive(Primitive::Double),
                            ..Structural::default()
                        },
                        Structural {
                            name: "latidute".into(),
                            ty: Ty::Primitive(Primitive::Double),
                            ..Structural::default()
                        }
                    ]),
                    ..Complex::default()
                }
            ]),
            entity_types: map(&[
                Entity {
                    name: "user".into(),
                    // key: "id".into(),
                    structural_props: map(&[
                        Structural {
                            name: "id".into(),
                            ..Structural::default()
                        },
                        Structural {
                            name: "username".into(),
                            ty: Ty::Primitive(Primitive::String),
                            ..Structural::default()
                        },
                        Structural {
                            name: "location".into(),
                            ty: Ty::Complex(DerefCell::with_state(ComplexPath(
                                namespace.clone(),
                                "coordinates".into()
                            ))),
                            ..Structural::default()
                        }
                    ]),
                    ..Entity::default()
                },
                Entity {
                    name: "application".into(),
                    key: Option::Some(vec![
                        Key {
                            alias: "id".into(),
                            ..Key::default()
                        }
                    ]),
                    // key: "id".into(),
                    structural_props: map(&[Structural {
                        name: "id".into(),
                        ..Structural::default()
                    }]),
                    navigation_props: map(&[Navigation {
                        name: "owner".into(),
                        ty: DerefCell::with_state(EntityPath(
                            namespace.clone(),
                            "user".to_string(),
                        )),
                        ..Navigation::default()
                    }]),
                    ..Entity::default()
                },
                Entity {
                    name: "device".into(),
                    // key: "id".into(),
                    structural_props: map(&[
                        Structural {
                            name: "id".into(),
                            ..Structural::default()
                        },
                        Structural {
                            name: "status".into(),
                            ty: Ty::Enum(DerefCell::with_state(EnumPath(
                                namespace.clone(),
                                "device state".into(),
                            ))),
                            ..Structural::default()
                        },
                    ]),
                    navigation_props: map(&[Navigation {
                        name: "owner".into(),
                        ty: DerefCell::with_state(EntityPath(
                            namespace.clone(),
                            "user".to_string(),
                        )),
                        ..Navigation::default()
                    }]),
                    ..Entity::default()
                },
            ]),
            entity_containers: map(&[EntityContainer {
                name: "balena".into(),
                entity_sets: map(&[EntitySet {
                    name: "applications".into(),
                    ty: DerefCell::with_state(EntityPath(
                        namespace.clone(),
                        "application".to_string(),
                    )),
                    ..EntitySet::default()
                }]),
                ..EntityContainer::default()
            }]),
            actions: map(&[
                Action {
                    name: "canAccess".into(),
                    ..Action::default()
                }
            ]),
            ..Schema::default()
        }]),
        ..Document::default()
    };

    doc.link();

    let model = AbstractModel {
        name: "DummyModel".into(),
        tables: vec![
            AbstractTable {
                name: "user".into(),
                sql_name: "user".into(),
                fields: vec![
                    AbstractColumn {
                        name: "id".into(),
                        _type: AbstractDataType::UUID {
                            default: Option::Some(UuidDefaults::V4_RANDOM)
                        },
                        not_null: true,
                        constraints: Option::Some(vec![AbstractTableColumnConstraintTypes::PRIMARY_KEY])
                    },
                    AbstractColumn {
                        name: "name".into(),
                        _type: AbstractDataType::VARCHAR {
                            length: 12
                        },
                        not_null: false,
                        constraints: Option::Some(vec![AbstractTableColumnConstraintTypes::UNIQUE_KEY])
                    },
                    AbstractColumn {
                        name: "actor".into(),
                        _type: AbstractDataType::UUID {
                            default: Option::None
                        },
                        not_null: false,
                        constraints: Option::None,
                    }
                ],
                constraints: vec![
                    AbstractTableConstraints {
                        name: "FK_ACTOR".into(),
                        _type: AbstractTableConstraintTypes::FOREIGN_KEY {
                            source_columns: vec!["actor".into()],
                            reference: "actor".into(),
                            reference_columns: vec!["id".into()],
                        }
                    }
                ],
            },
            AbstractTable {
                name: "actor".into(),
                sql_name: "actor".into(),
                fields: vec![
                    AbstractColumn {
                        name: "id".into(),
                        _type: AbstractDataType::UUID {
                            default: Option::Some(UuidDefaults::V4_RANDOM)
                        },
                        not_null: true,
                        constraints: Option::Some(vec![AbstractTableColumnConstraintTypes::PRIMARY_KEY])
                    },
                    AbstractColumn {
                        name: "name".into(),
                        _type: AbstractDataType::VARCHAR {
                            length: 12
                        },
                        not_null: false,
                        constraints: Option::None,
                    }
                ],
                constraints: vec![],
            }
        ]
    };

    generate_ddl(&model, &crate::db::postgres::engine::PostgresEngine::new());
    //let j = csdl::generate_csdl_json(&doc).unwrap();

    // Print, write to a file, or send to an HTTP server.
    //println!("{}", j);

    //let mut p = parser::Parser::new(&doc);
    // // println!("{:?}", parser::odataRelativeUri("ProductsByComplex(complex=@c)?@c={\"@odata.type\":\"doc.Customer\",\"Name\":\"Value\"}\n"));
    // // println!("{:#?}", p.parse("https://example.com/foobar/users/$filter=@foo/$filter=@bar/$count?$filter=true eq true"));
    //let (_, tree) = p
    //   .parse("http://example.com/foobar/applications(true)")
    //    .unwrap();

    //println!("{:#?}", tree);
    // // println!("{:#?}", compiler::compile_sql(&tree, &doc));
}
