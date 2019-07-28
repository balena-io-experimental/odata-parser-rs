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

	let mut application = schema::kind::Entity{
		name: String::from("application"),
		key: Some(vec![String::from("id")]),
		base_type: None,
		is_abstract: false,
		open_type: false,
		has_stream: false,
		properties: HashMap::new(),
	};

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

	application.properties.insert(property.name.clone(), schema::property::Property::Structural(property));

	let property = schema::property::Navigation{
		name: String::from("owner"),
		kind: user.clone(),
		collection: false,
		nullable: false,
		partner: String::from("foobar"),
		contains_target: false,
		on_delete: schema::property::OnDeleteAction::None,
	};

	application.properties.insert(property.name.clone(), schema::property::Property::Navigation(property));

	doc.schema.members.insert(user.name.clone(), schema::SchemaMember::Entity(user.clone()));
	doc.schema.members.insert(application.name.clone(), schema::SchemaMember::Entity(application.clone()));

	let entity_set = schema::EntitySet{
		name: String::from("applications"),
		include_in_service_document: true,
		kind: application.clone(),
	};

	let entity_container = doc.schema.get_entity_container_mut();

	entity_container.members.insert(entity_set.name.clone(), schema::EntityContainerMember::EntitySet(entity_set.clone()));


	println!("{:#?}", doc);

	let mut p = parser::Parser::new(&doc);
	// println!("{:?}", parser::odataRelativeUri("ProductsByComplex(complex=@c)?@c={\"@odata.type\":\"Model.Customer\",\"Name\":\"Value\"}\n"));
	// println!("{:#?}", p.parse("https://example.com/foobar/users/$filter=@foo/$filter=@bar/$count?$filter=true eq true"));
	let (_, tree) = p.parse("https://example.com/foobar/applications/namespace.foo/$filter=@a/$filter=@b/foo.bar(null)").unwrap();

	println!("{:#?}", tree);
	// println!("{:#?}", compiler::compile_sql(&tree, &doc));
}

