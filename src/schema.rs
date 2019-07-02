//FIXME
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

use std::collections::HashMap;

#[derive(Clone,Debug,Eq,PartialEq)]
pub enum PrimitiveType {
	Binary,
	Boolean,
	Byte,
	Date,
	DateTimeOffset,
	Decimal,
	Double,
	Duration,
	Guid,
	Int16,
	Int32,
	Int64,
	SByte,
	Single,
	Stream,
	String,
	TimeOfDay,
	Geography,
	GeographyPoint,
	GeographyLineString,
	GeographyPolygon,
	GeographyMultiPoint,
	GeographyMultiLineString,
	GeographyMultiPolygon,
	GeographyCollection,
	Geometry,
	GeometryPoint,
	GeometryLineString,
	GeometryPolygon,
	GeometryMultiPoint,
	GeometryMultiLineString,
	GeometryMultiPolygon,
	GeometryCollection,
}

#[derive(Debug,Eq,PartialEq)]
pub enum AbstractType {
	PrimitiveType,
	ComplexType,
	EntityType,
	Untyped,
}

#[derive(Debug,Eq,PartialEq)]
pub enum VocabularyType {
	AnnotationPath,
	PropertyPath,
	NavigationPropertyPath,
	AnyPropertyPath,
	ModelElementPath,
}

#[derive(Debug,Eq,PartialEq)]
pub struct Document {
	pub version: String,
	pub reference: Option<Reference>,
	pub namespace: Namespace,
	pub schema: Schema,
	pub service_root: String,
}

impl Document {
	pub fn new() -> Self {
		return Self{
			version: String::from("4.01"),
			reference: None,
			namespace: String::from("foo.org"),
			service_root: String::from("https://example.com/foobar/"),
			schema: Schema{
				namespace: String::from("foo.org"),
				alias: None,
				entity_container_name: None,
				members: HashMap::new(),
			},
		}
	}
}

type Reference = String;

type Namespace = String;

type Identifier = String;

#[derive(Debug,Eq,PartialEq)]
pub struct Schema {
	pub namespace: Namespace,
	pub alias: Option<Identifier>,
	entity_container_name: Option<Identifier>,
	pub members: HashMap<Identifier, SchemaMember>,
}

impl Schema {
	pub fn get_entity_container(&self) -> &EntityContainer {
		match &self.entity_container_name {
			None => panic!(),
			Some(name) => match self.members.get(name).unwrap() {
				SchemaMember::EntityContainer(c) => c,
				_ => panic!(),
			},
		}
	}

	pub fn get_entity_container_mut(&mut self) -> &mut EntityContainer {
		match &self.entity_container_name {
			None => panic!(),
			Some(name) => match self.members.get_mut(name).unwrap() {
				SchemaMember::EntityContainer(c) => c,
				_ => panic!(),
			},
		}
	}

	pub fn set_entity_container(&mut self, member: EntityContainer) {
		self.entity_container_name = Some(member.name.clone());
		self.members.insert(member.name.clone(), SchemaMember::EntityContainer(member));
	}
}

#[derive(Debug,Eq,PartialEq)]
pub enum SchemaMember {
	EntityType(EntityType),
	ComplexType,
	EnumerationType,
	TypeDefinition,
	Action,
	Function,
	EntityContainer(EntityContainer),
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub struct EntityType {
	pub name: String,
	pub key: Option<Vec<Identifier>>,
	pub base_type: Option<Identifier>,
	pub is_abstract: bool,
	pub open_type: bool,
	pub has_stream: bool,
	pub properties: HashMap<Identifier, Property>,
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub enum Property {
	StructuralProperty(StructuralProperty),
	NavigationProperty(NavigationProperty),
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub enum PropertyType {
	PrimitiveType(PrimitiveType),
	ComplexType(ComplexType),
	EnumerationType(EnumerationType),
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub struct StructuralProperty {
	pub name: Identifier,
	pub kind: PropertyType,
	pub collection: bool,
	pub nullable: bool,
	pub max_length: u32,
	pub unicode: bool,
	pub precision: u32,
	pub scale: u32,
	pub srid: u32,
	// pub default_value: T,
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub struct NavigationProperty {
	pub name: Identifier,
	pub kind: Identifier,
	pub collection: bool,
	pub nullable: bool,
	pub partner: Identifier,
	pub contains_target: bool,
	// FIXME pub referential_constraint: HashMap<&'a Property<'a>, &'a Property<'a>>,
	pub on_delete: OnDeleteAction,
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub enum OnDeleteAction {
	Cascade,
	None,
	SetNull,
	SetDefault,
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub struct ComplexType {
	pub name: String,
	pub base_type: Option<Identifier>,
	pub is_abstract: bool,
	pub open_type: bool,
	pub properties: HashMap<Identifier, Property>,
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub struct EnumerationType {
	pub name: String,
	pub underlying_type: EnumerationUnderlyingType,
	pub is_flags: bool,
	pub members: HashMap<Identifier, i64>,
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub enum EnumerationUnderlyingType {
	Byte,
	SByte,
	Int16,
	Int32,
	Int64,
}

#[derive(Debug,Eq,PartialEq)]
pub struct TypeDefinition {
	pub name: Identifier,
	pub underlying_type: PrimitiveType,
	pub max_length: u32,
	pub unicode: bool,
	pub precision: u32,
	pub scale: u32,
	pub srid: u32
}

#[derive(Debug,Eq,PartialEq)]
pub struct Action {
	pub name: Identifier,
	pub is_bound: bool,
	pub entity_set_path: String,
	pub parameter: Vec<Parameter>,
	pub return_type: Option<ReturnType>,
}

#[derive(Debug,Eq,PartialEq)]
pub struct Function {
	pub name: Identifier,
	pub is_bound: bool,
	pub entity_set_path: String,
	pub parameter: Vec<Parameter>,
	pub return_type: ReturnType,
	pub is_composable: bool,
}

#[derive(Debug,Eq,PartialEq)]
pub struct ReturnType {
	pub kind: Identifier,
	pub collection: bool,
	pub nullable: bool,
	pub max_length: u32,
	pub unicode: bool,
	pub precision: u32,
	pub scale: u32,
	pub srid: u32,
}

#[derive(Debug,Eq,PartialEq)]
pub struct Parameter {
	pub name: Identifier,
	pub kind: Identifier,
	pub collection: bool,
	pub nullable: bool,
	pub max_length: u32,
	pub unicode: bool,
	pub precision: u32,
	pub scale: u32,
	pub srid: u32,
}

#[derive(Debug,Eq,PartialEq)]
pub struct EntityContainer {
	pub name: Identifier,
	pub extends: Option<Identifier>,
	pub members: HashMap<Identifier, EntityContainerMember>,
}

#[derive(Debug,Eq,PartialEq)]
pub enum EntityContainerMember {
	EntitySet(EntitySet),
	Singleton,
	ActionImport,
	FunctionImport,
}

#[derive(Clone,Debug,Eq,PartialEq)]
pub struct EntitySet {
	pub name: Identifier,
	pub include_in_service_document: bool,
	pub kind: EntityType,
}

#[derive(Debug,Eq,PartialEq)]
pub struct Singleton {
	pub kind: Identifier,
}

#[derive(Debug,Eq,PartialEq)]
pub struct ActionImport {
	pub action: Identifier,
	pub entity_set: Identifier,
}

#[derive(Debug,Eq,PartialEq)]
pub struct FunctionImport {
	pub function: Identifier,
	pub entity_set: Identifier,
	pub include_in_service_document: bool,
}
