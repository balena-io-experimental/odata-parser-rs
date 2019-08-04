#![recursion_limit = "128"]

#[macro_use]
extern crate nom;

mod ast;
mod derefcell;
mod parser;
mod schema;

use derefcell::DerefCell;
use schema::map;
use schema::property::{Navigation, Structural};
use schema::ty::*;
use schema::{Document, EntityContainer, EntityContainerPath, EntitySet, Schema};

fn main() {
    let namespace = "com.balena_cloud.api".to_string();

    let doc = Document {
        service_root: "http://example.com/foobar/".into(),
        entity_container: DerefCell::with_state(EntityContainerPath(
            namespace.clone(),
            "balena".to_string(),
        )),
        schemas: map(&[Schema {
            namespace: namespace.clone(),
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
                    ]),
                    ..Entity::default()
                },
                Entity {
                    name: "application".into(),
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
            ..Schema::default()
        }]),
        ..Document::default()
    };

    doc.link();

    let mut p = parser::Parser::new(&doc);
    // // println!("{:?}", parser::odataRelativeUri("ProductsByComplex(complex=@c)?@c={\"@odata.type\":\"doc.Customer\",\"Name\":\"Value\"}\n"));
    // // println!("{:#?}", p.parse("https://example.com/foobar/users/$filter=@foo/$filter=@bar/$count?$filter=true eq true"));
    let (_, tree) = p
        .parse("http://example.com/foobar/applications(true)")
        .unwrap();

    println!("{:#?}", tree);
    // // println!("{:#?}", compiler::compile_sql(&tree, &doc));
}
