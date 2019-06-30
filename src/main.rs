#![recursion_limit="128"]

#[macro_use]
extern crate nom;

mod parser;
mod ast;
mod schema;

use std::collections::HashMap;

fn main() {
	let mut doc = schema::Document::new();

	doc.schema.set_entity_container(schema::EntityContainer{
		name: String::from("foobar"),
		extends: None,
		members: HashMap::new(),
	});

	let mut user = schema::EntityType{
		name: String::from("user"),
		key: Some(vec![String::from("id")]),
		base_type: None,
		is_abstract: false,
		open_type: false,
		has_stream: false,
		properties: HashMap::new(),
	};

	let property = schema::StructuralProperty{
		name: String::from("username"),
		kind: schema::PropertyType::PrimitiveType(schema::PrimitiveType::String),
		collection: false,
		nullable: false,
		max_length: 20,
		unicode: false,
		precision: 0,
		scale: 0,
		srid: 0,
	};

	user.properties.insert(property.name.clone(), schema::Property::StructuralProperty(property));

	doc.schema.members.insert(user.name.clone(), schema::SchemaMember::EntityType(user.clone()));

	let entity_set = schema::EntitySet{
		name: String::from("users"),
		include_in_service_document: true,
		kind: user.clone(),
	};

	let mut entity_container = doc.schema.get_entity_container_mut();

	entity_container.members.insert(entity_set.name.clone(), schema::EntityContainerMember::EntitySet(entity_set.clone()));


	println!("{:#?}", doc);

	// println!("{:?}", parser::odataRelativeUri("ProductsByComplex(complex=@c)?@c={\"@odata.type\":\"Model.Customer\",\"Name\":\"Value\"}\n"));
	println!("{:#?}", parser::odataUri("https://example.com/foobar/users\n", &doc.schema));
}

