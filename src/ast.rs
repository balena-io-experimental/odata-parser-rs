//FIXME
#![allow(dead_code)]

use super::schema;

#[derive(Debug,Clone)]
pub struct ODataURI<'a> {
	pub service_root: &'a str,
	pub relative_uri: RelativeURI<'a>,
}

#[derive(Debug,Clone)]
pub struct ServiceRoot<'a> {
	pub data: &'a str,
}

#[derive(Debug,Clone)]
pub enum RelativeURI<'a> {
	None,
	Batch(Option<Vec<QueryOption<'a>>>),
	Entity,
	Metadata,
	Resource(ResourcePath<'a>),
}

#[derive(Debug,Clone)]
pub struct ResourcePath<'a> {
	pub segments: Vec<PathSegment<'a>>,
}

#[derive(Debug,Clone)]
pub enum PathSegment<'a> {
	EntitySet(&'a schema::EntitySet),             // -> collectionNavigation
	Singleton,                                    // -> singleNavigation
	Action,                                       // -> ()
	Function,                                     // -> collection,single,complexCollection,complex,primitiveCollection,primitive,() navigation
	Crossjoin,                                    // -> ()
	All,                                          // -> ()
	Cast,
	BoundOperation,                               // -> ()
	Count,                                        // -> ()
	Each,                                         // -> boundOperation
	Filter(ParameterAlias<'a>),                   // -> collectionNavigation
	KeyPredicate(KeyPredicate<'a>),               // -> singleNavigation
	Property(&'a schema::property::Property),     // -> single,collection,complexCollection,complex,primitiveCollection,primitive,boundOperation
	Ref,                                          // -> ()
	Value,
	OrdinalIndex(i64),
}

#[derive(Debug,Clone)]
pub struct KeyPredicate<'a> {
	pub values: Vec<(KeyProperty<'a>)>,
}

#[derive(Debug,Clone)]
pub struct KeyProperty<'a> {
	pub property: &'a schema::property::Property,
	pub value: KeyValue<'a>,
}

#[derive(Debug,Clone)]
pub enum KeyValue<'a> {
	ParameterAlias(ParameterAlias<'a>),
	Value(&'a str),
}

#[derive(Debug,Clone)]
pub struct PrimitiveValue<'a> {
	kind: schema::kind::Primitive,
	value: &'a str,
}

#[derive(Debug,Clone)]
pub struct ParameterAlias<'a> {
	pub name: &'a str,
}

#[derive(Debug,Clone)]
pub enum FormatKind<'a> {
	JSON,
	Atom,
	XML,
	Custom(&'a str),
}

#[derive(Debug,Clone)]
pub enum QueryOption<'a> {
	Format(FormatKind<'a>),
	Custom(&'a str),
}
