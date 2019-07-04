#![recursion_limit="128"]

#[macro_use]
extern crate nom;

pub mod parser;
pub mod ast;
mod schema;

use std::collections::HashMap;

fn main() {
	let mut doc = schema::Document::new();

	doc.schema.set_entity_container(schema::EntityContainer{
		name: String::from("foobar"),
		extends: None,
		members: HashMap::new(),
	});

	let mut user = schema::kind::Entity{
		name: String::from("user"),
		key: Some(vec![String::from("id")]),
		base_type: None,
		is_abstract: false,
		open_type: false,
		has_stream: false,
		properties: HashMap::new(),
	};

	let property = schema::property::Structural{
		name: String::from("username"),
		kind: schema::property::Type::Primitive(schema::kind::Primitive::String),
		collection: false,
		nullable: false,
		max_length: 20,
		unicode: false,
		precision: 0,
		scale: 0,
		srid: 0,
	};

	user.properties.insert(property.name.clone(), schema::property::Property::Structural(property));

	let property = schema::property::Structural{
		name: String::from("id"),
		kind: schema::property::Type::Primitive(schema::kind::Primitive::Int64),
		collection: false,
		nullable: false,
		max_length: 20,
		unicode: false,
		precision: 0,
		scale: 0,
		srid: 0,
	};

	user.properties.insert(property.name.clone(), schema::property::Property::Structural(property));

	doc.schema.members.insert(user.name.clone(), schema::SchemaMember::Entity(user.clone()));

	let entity_set = schema::EntitySet{
		name: String::from("users"),
		include_in_service_document: true,
		kind: user.clone(),
	};

	let entity_container = doc.schema.get_entity_container_mut();

	entity_container.members.insert(entity_set.name.clone(), schema::EntityContainerMember::EntitySet(entity_set.clone()));


	println!("{:#?}", doc);

	let mut p = parser::Parser::new(&doc);
	// println!("{:?}", parser::odataRelativeUri("ProductsByComplex(complex=@c)?@c={\"@odata.type\":\"Model.Customer\",\"Name\":\"Value\"}\n"));
	// println!("{:#?}", p.parse("https://example.com/foobar/users/$filter=@foo/$filter=@bar/$count"));
	println!("{:#?}", p.parse("https://example.com/foobar/users/123"));
}

