//FIXME
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

use std::collections::HashMap;
use std::default::Default;
use std::fmt;
use std::iter::FromIterator;
use serde::{Serialize, Serializer};

use super::derefcell::DerefCell;
use super::derefcell::Resolve;

use crate::util::{bool_is, bool_not};

#[derive(Debug, Eq, PartialEq)]
pub enum VocabularyType {
    AnnotationPath,
    PropertyPath,
    NavigationPropertyPath,
    AnyPropertyPath,
    ModelElementPath,
}

pub trait Named {
    fn name(&self) -> &str;
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Document<'a> {
    #[serde(rename = "$Version")]
    pub version: String,
    #[serde(skip)]
    pub reference: Option<Vec<DerefCell<'a, Document<'a>>>>,
    #[serde(rename = "$EntityContainer")]
    pub entity_container: DerefCell<'a, EntityContainer<'a>, EntityContainerPath>,
    #[serde(flatten)]
    pub schemas: HashMap<String, Schema<'a>>,
    #[serde(skip)]
    pub service_root: String,
}

impl<'a> Document<'a> {
    pub fn link(&'a self) {
        self.entity_container.link(self);
        for schema in self.schemas.values() {
            schema.parent.link(self);
            for entity_type in schema.entity_types.values() {
                entity_type.parent.link(&schema);
                /*if let Some(key) = &entity_type.key {
                    for (_, property) in key {
                        // property.link(self.resolve(property.state()));
                    }
                }*/
                if let Some(base) = &entity_type.base_type {
                    base.link(self);
                }
                for property in entity_type.structural_props.values() {
                    property.parent.unwrap_entity_type().link(&entity_type);
                    property.ty.link(self);
                }
                for property in entity_type.navigation_props.values() {
                    property.parent.unwrap_entity_type().link(&entity_type);
                    property.ty.link(self);
                }
            }
            for entity_container in schema.entity_containers.values() {
                entity_container.parent.link(&schema);
                for entity_set in entity_container.entity_sets.values() {
                    entity_set.parent.link(&entity_container);
                    entity_set.ty.link(self);
                }
            }
        }
    }
}

impl Default for Document<'_> {
    fn default() -> Self {
        Self {
            version: String::from("1"),
            reference: None,
            // FIXME
            entity_container: DerefCell::with_state(EntityContainerPath(
                "foo".to_string(),
                "bar".to_string(),
            )),
            schemas: HashMap::new(),
            service_root: String::from("http://example.org"),
        }
    }
}

type Reference = String;

type Namespace = String;

pub type Identifier = String;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Schema<'a> {
    #[serde(skip)]
    pub namespace: Namespace,
    #[serde(skip)]
    pub parent: DerefCell<'a, Document<'a>>,
    #[serde(rename = "$Alias", skip_serializing_if = "Option::is_none")]
    pub alias: Option<Identifier>,
    #[serde(flatten)]
    pub entity_types: HashMap<Identifier, ty::Entity<'a>>,
    #[serde(flatten)]
    pub complex_types: HashMap<Identifier, ty::Complex<'a>>,
    #[serde(flatten)]
    pub enum_types: HashMap<Identifier, ty::Enum<'a>>,
    #[serde(flatten)]
    pub type_definitions: HashMap<Identifier, ty::Definition<'a>>,
    #[serde(flatten)]
    pub actions: HashMap<Identifier, Action<'a>>,
    #[serde(flatten)]
    pub functions: HashMap<Identifier, Function<'a>>,
    #[serde(flatten)]
    pub entity_containers: HashMap<Identifier, EntityContainer<'a>>,
}

impl Named for Schema<'_> {
    fn name(&self) -> &str {
        &self.namespace
    }
}

impl Default for Schema<'_> {
    fn default() -> Self {
        Schema {
            namespace: String::from(""),
            parent: DerefCell::new(),
            alias: None,
            entity_types: HashMap::new(),
            complex_types: HashMap::new(),
            enum_types: HashMap::new(),
            type_definitions: HashMap::new(),
            actions: HashMap::new(),
            functions: HashMap::new(),
            entity_containers: HashMap::new(),
        }
    }
}

pub mod ty {
    use super::{property, DerefCell, Document, Identifier, Named, Resolve};
    use std::collections::HashMap;
    use serde::{Serialize, Serializer};
    use serde::ser::{SerializeMap};
    use crate::util::{bool_not, bool_is, u32_is_zero};

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum Ty<'a> {
        Primitive(Primitive),
        Entity(DerefCell<'a, Entity<'a>, EntityPath>),
        Complex(DerefCell<'a, Complex<'a>, ComplexPath>),
        Enum(DerefCell<'a, Enum<'a>, EnumPath>),
        Definition(DerefCell<'a, Definition<'a>, DefinitionPath>),
    }

    impl<'a> Serialize for Ty<'a> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
        {
            match self {
                Ty::Primitive(primitive) => {
                    primitive.serialize(serializer)
                },
                Ty::Entity(entity) => {
                    entity.serialize(serializer)
                },
                Ty::Complex(complex) => {
                    complex.serialize(serializer)
                },
                Ty::Enum(enumaration) => {
                    enumaration.serialize(serializer)
                }
                _ => {
                    serializer.serialize_str("TEST")
                }
            }
        }
    }

    pub enum TyRef<'a> {
        Primitive(Primitive),
        Entity(&'a Entity<'a>),
        Complex(&'a Complex<'a>),
        Enum(&'a Enum<'a>),
        Definition(&'a Definition<'a>),
    }

    impl<'a> Ty<'a> {
        pub fn borrow(&'a self) -> TyRef<'a> {
            match self {
                Ty::Primitive(t) => TyRef::Primitive(*t),
                Ty::Entity(t) => TyRef::Entity(t),
                Ty::Complex(t) => TyRef::Complex(t),
                Ty::Enum(t) => TyRef::Enum(t),
                Ty::Definition(t) => TyRef::Definition(t),
            }
        }

        pub fn link(&self, doc: &'a Document<'a>) {
            match self {
                Ty::Primitive(_) => {}
                Ty::Entity(e) => e.link(doc),
                Ty::Complex(e) => e.link(doc),
                Ty::Enum(e) => e.link(doc),
                Ty::Definition(e) => e.link(doc),
            }
        }
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize)]
    pub enum Primitive {
        #[serde(rename = "Edm.Binary")]
        Binary,
        #[serde(rename = "Edm.Boolean")]
        Boolean,
        #[serde(rename = "Edm.Byte")]
        Byte,
        #[serde(rename = "Edm.Date")]
        Date,
        #[serde(rename = "Edm.DateTimeOffset")]
        DateTimeOffset,
        #[serde(rename = "Edm.Decimal")]
        Decimal,
        #[serde(rename = "Edm.Double")]
        Double,
        #[serde(rename = "Edm.Duration")]
        Duration,
        #[serde(rename = "Edm.Guid")]
        Guid,
        #[serde(rename = "Edm.Int16")]
        Int16,
        #[serde(rename = "Edm.Int32")]
        Int32,
        #[serde(rename = "Edm.Int64")]
        Int64,
        #[serde(rename = "Edm.SByte")]
        SByte,
        #[serde(rename = "Edm.Single")]
        Single,
        #[serde(rename = "Edm.Stream")]
        Stream,
        #[serde(rename = "Edm.String")]
        String,
        #[serde(rename = "Edm.TimeOfDay")]
        TimeOfDay,
        #[serde(rename = "Edm.Geography")]
        Geography,
        #[serde(rename = "Edm.GeographyPoint")]
        GeographyPoint,
        #[serde(rename = "Edm.GeographyLineString")]
        GeographyLineString,
        #[serde(rename = "Edm.GeographyPolygon")]
        GeographyPolygon,
        #[serde(rename = "Edm.GeographyMultiPoint")]
        GeographyMultiPoint,
        #[serde(rename = "Edm.GeographyMultiLineString")]
        GeographyMultiLineString,
        #[serde(rename = "Edm.GeographyMultiPolygon")]
        GeographyMultiPolygon,
        #[serde(rename = "Edm.GeographyCollection")]
        GeographyCollection,
        #[serde(rename = "Edm.Geometry")]
        Geometry,
        #[serde(rename = "Edm.GeometryPoint")]
        GeometryPoint,
        #[serde(rename = "Edm.GeometryLineString")]
        GeometryLineString,
        #[serde(rename = "Edm.GeometryPolygon")]
        GeometryPolygon,
        #[serde(rename = "Edm.GeometryMultiPoint")]
        GeometryMultiPoint,
        #[serde(rename = "Edm.GeometryMultiLineString")]
        GeometryMultiLineString,
        #[serde(rename = "Edm.GeometryMultiPolygon")]
        GeometryMultiPolygon,
        #[serde(rename = "Edm.GeometryCollection")]
        GeometryCollection,
    }

    #[derive(Debug, Eq, PartialEq, Serialize)]
    pub enum Abstract {
        #[serde(rename = "Edm.PrimitiveType")]
        Primitive,
        #[serde(rename = "Edm.ComplexType")]
        Complex,
        #[serde(rename = "Edm.EntityType")]
        Entity,
        #[serde(rename = "Edm.Untyped")]
        Untyped,
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub struct Entity<'a> {
        #[serde(skip)]
        pub name: String,
        #[serde(rename = "$Kind", default= "EntityType")]
        pub kind: String,
        #[serde(skip)]
        pub parent: DerefCell<'a, super::Schema<'a>>,
        #[serde(rename = "$Key", skip_serializing_if = "Option::is_none")]
        pub key: Option<Vec<Key>>,
        #[serde(rename = "$BaseType", skip_serializing_if = "Option::is_none")]
        pub base_type: Option<DerefCell<'a, Entity<'a>, EntityPath>>,
        #[serde(rename = "$Abstract", skip_serializing_if = "bool_not")]
        pub is_abstract: bool,
        #[serde(rename = "$OpenType", skip_serializing_if = "bool_not")]
        pub open_type: bool,
        #[serde(rename = "$HasStream", skip_serializing_if = "bool_not")]
        pub has_stream: bool,
        #[serde(flatten)]
        pub structural_props: HashMap<Identifier, property::Structural<'a>>,
        #[serde(flatten)]
        pub navigation_props: HashMap<Identifier, property::Navigation<'a>>,
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct Key {
        pub alias: String,
        pub alias_path: String,
    }

    impl Default for Key {
        fn default() -> Self {
            Key {
                alias: String::from("alias"),
                alias_path: String::default(),
            }
        }
    }

    impl Serialize for Key {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
        {
            if self.alias_path.is_empty() {
                return serializer.serialize_str(&self.alias.as_str());
            }
            let mut map = serializer.serialize_map(None)?;
            map.serialize_entry(&self.alias.as_str(), &self.alias_path.as_str())?;
            map.end()
        }
    }

    impl Named for Entity<'_> {
        fn name(&self) -> &str {
            &self.name
        }
    }

    impl Default for Entity<'_> {
        fn default() -> Self {
            Entity {
                name: String::from(""),
                kind: String::from("EntityType"),
                parent: DerefCell::new(),
                key: None,
                base_type: None,
                is_abstract: false,
                open_type: false,
                has_stream: false,
                structural_props: HashMap::new(),
                navigation_props: HashMap::new(),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct EntityPath(pub String, pub String);

    impl Serialize for EntityPath {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
        {
            serializer.serialize_str(format!("{}.{}", self.0, self.1).as_str())
        }
    }

    impl<'a> Resolve<'a, Entity<'a>> for EntityPath {
        type Root = Document<'a>;

        fn resolve(&self, root: &'a Self::Root) -> &'a Entity<'a> {
            root.schemas
                .get(&self.0)
                .unwrap()
                .entity_types
                .get(&self.1)
                .unwrap()
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub struct Complex<'a> {
        #[serde(skip)]
        pub name: String,
        #[serde(rename = "$Kind", default= "ComplexType")]
        pub kind: String,
        #[serde(skip)]
        pub parent: DerefCell<'a, super::Schema<'a>>,
        #[serde(rename = "$BaseType", skip_serializing_if = "Option::is_none")]
        pub base_type: Option<DerefCell<'a, Complex<'a>, ComplexPath>>,
        #[serde(rename = "$Abstract", skip_serializing_if = "bool_not")]
        pub is_abstract: bool,
        #[serde(rename = "$OpenType", skip_serializing_if = "bool_not")]
        pub open_type: bool,
        #[serde(flatten)]
        pub structural_props: HashMap<Identifier, property::Structural<'a>>,
        #[serde(flatten)]
        pub navigation_props: HashMap<Identifier, property::Navigation<'a>>,
    }

    impl Named for Complex<'_> {
        fn name(&self) -> &str {
            &self.name
        }
    }

    impl Default for Complex<'_> {
        fn default() -> Self {
            Complex {
                name: String::from(""),
                kind: String::from("ComplexType"),
                parent: DerefCell::new(),
                base_type: None,
                is_abstract: false,
                open_type: false,
                structural_props: HashMap::new(),
                navigation_props: HashMap::new(),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct ComplexPath(pub String, pub String);

    impl Serialize for ComplexPath {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
        {
            serializer.serialize_str(format!("{}.{}", self.0, self.1).as_str())
        }
    }

    impl<'a> Resolve<'a, Complex<'a>> for ComplexPath {
        type Root = Document<'a>;

        fn resolve(&self, root: &'a Self::Root) -> &'a Complex<'a> {
            root.schemas
                .get(&self.0)
                .unwrap()
                .complex_types
                .get(&self.1)
                .unwrap()
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub struct Enum<'a> {
        #[serde(skip)]
        pub name: String,
        #[serde(rename = "$Kind", default= "EnumType")]
        pub kind: String,
        #[serde(skip)]
        pub parent: DerefCell<'a, super::Schema<'a>>,
        #[serde(rename = "$UnderlyingType")]
        pub underlying_type: EnumBase,
        #[serde(rename = "$IsFlags")]
        pub is_flags: bool,
        #[serde(flatten)]
        pub members: HashMap<Identifier, i64>,
    }

    impl Named for Enum<'_> {
        fn name(&self) -> &str {
            &self.name
        }
    }

    impl Default for Enum<'_> {
        fn default() -> Self {
            Enum {
                name: String::from(""),
                kind: String::from("EnumType"),
                parent: DerefCell::new(),
                underlying_type: EnumBase::Int16,
                is_flags: false,
                members: HashMap::new(),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct EnumPath(pub String, pub String);

    impl Serialize for EnumPath {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: Serializer,
        {
            serializer.serialize_str(format!("{}.{}", self.0, self.1).as_str())
        }
    }

    impl<'a> Resolve<'a, Enum<'a>> for EnumPath {
        type Root = Document<'a>;

        fn resolve(&self, root: &'a Self::Root) -> &'a Enum<'a> {
            root.schemas
                .get(&self.0)
                .unwrap()
                .enum_types
                .get(&self.1)
                .unwrap()
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub enum EnumBase {
        #[serde(rename = "Edm.Byte")]
        Byte,
        #[serde(rename = "Edm.SByte")]
        SByte,
        #[serde(rename = "Edm.Int16")]
        Int16,
        #[serde(rename = "Edm.Int32")]
        Int32,
        #[serde(rename = "Edm.Int64")]
        Int64,
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub struct Definition<'a> {
        #[serde(skip)]
        pub name: Identifier,
        #[serde(rename = "$Kind", default= "TypeDefinition")]
        pub kind: String,
        #[serde(skip)]
        pub parent: DerefCell<'a, super::Schema<'a>>,
        #[serde(rename = "$Type")]
        pub underlying_type: Primitive,
        #[serde(rename = "$MaxLength", skip_serializing_if = "u32_is_zero")]
        pub max_length: u32,
        #[serde(rename = "$Unicode", skip_serializing_if = "bool_is")]
        pub unicode: bool,
        #[serde(rename = "$Precision", skip_serializing_if = "u32_is_zero")]
        pub precision: u32,
        #[serde(rename = "$Scale", skip_serializing_if = "u32_is_zero")]
        pub scale: u32,
        #[serde(rename = "$SRID", skip_serializing_if = "u32_is_zero")]
        pub srid: u32,
    }

    impl Default for Definition<'_> {
        fn default() -> Self {
            Definition {
                name: String::from(""),
                kind: String::from("TypeDefinition"),
                parent: DerefCell::new(),
                underlying_type: Primitive::Int16,
                unicode: true,
                max_length: 0,
                precision: 0,
                scale: 0,
                srid: 0,
            }
        }
    }

    impl Named for Definition<'_> {
        fn name(&self) -> &str {
            &self.name
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub struct DefinitionPath(pub String, pub String);

    impl<'a> Resolve<'a, Definition<'a>> for DefinitionPath {
        type Root = Document<'a>;

        fn resolve(&self, root: &'a Self::Root) -> &'a Definition<'a> {
            root.schemas
                .get(&self.0)
                .unwrap()
                .type_definitions
                .get(&self.1)
                .unwrap()
        }
    }
}

pub mod property {
    use super::ty;
    use super::DerefCell;
    use super::Identifier;
    use super::Named;
    use serde::{Serialize};
    use crate::util::{bool_not, bool_is, u32_is_zero};

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub enum Parent<'a> {
        Entity(DerefCell<'a, ty::Entity<'a>>),
        Complex(DerefCell<'a, ty::Complex<'a>>),
    }

    impl<'a> Parent<'a> {
        pub fn unwrap_entity_type(&'a self) -> &'a DerefCell<'a, ty::Entity<'a>> {
            match self {
                Parent::Entity(e) => e,
                _ => panic!("Parent was not an entity type"),
            }
        }

        pub fn unwrap_complex_type(&'a self) -> &'a DerefCell<'a, ty::Complex<'a>> {
            match self {
                Parent::Complex(c) => c,
                _ => panic!("Parent was not a complex type"),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub enum Property<'a> {
        Structural(DerefCell<'a, Structural<'a>>),
        Navigation(DerefCell<'a, Navigation<'a>>),
    }

    pub enum PropertyRef<'a> {
        Structural(&'a Structural<'a>),
        Navigation(&'a Navigation<'a>),
    }

    impl<'a> Property<'a> {
        pub fn borrow(&'a self) -> PropertyRef<'a> {
            match self {
                Property::Structural(p) => PropertyRef::Structural(p),
                Property::Navigation(p) => PropertyRef::Navigation(p),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub struct Structural<'a> {
        #[serde(skip)]
        pub name: Identifier,
        #[serde(skip)]
        pub parent: Parent<'a>,
        #[serde(rename = "$Type")]
        pub ty: ty::Ty<'a>,
        #[serde(rename = "$Collection", skip_serializing_if = "bool_not")]
        pub collection: bool,
        #[serde(rename = "$Nullable", skip_serializing_if = "bool_not")]
        pub nullable: bool,
        #[serde(rename = "$MaxLength", skip_serializing_if = "u32_is_zero")]
        pub max_length: u32,
        #[serde(rename = "$Unicode", skip_serializing_if = "bool_is")]
        pub unicode: bool,
        #[serde(rename = "$Precision", skip_serializing_if = "u32_is_zero")]
        pub precision: u32,
        #[serde(rename = "$Scale", skip_serializing_if = "u32_is_zero")]
        pub scale: u32,
        #[serde(rename = "$SRID", skip_serializing_if = "u32_is_zero")]
        pub srid: u32,
        // pub default_value: T,
    }

    impl Named for Structural<'_> {
        fn name(&self) -> &str {
            &self.name
        }
    }

    impl Default for Structural<'_> {
        fn default() -> Self {
            Structural {
                name: String::from(""),
                parent: Parent::Entity(DerefCell::new()),
                ty: ty::Ty::Primitive(ty::Primitive::Int64),
                collection: false,
                nullable: false,
                max_length: 0,
                unicode: true,
                precision: 0,
                scale: 0,
                srid: 0,
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub struct Navigation<'a> {
        #[serde(skip)]
        pub name: Identifier,
        #[serde(skip)]
        pub parent: Parent<'a>,
        #[serde(rename = "$Kind", default= "NavigationProperty")]
        pub kind: String,
        #[serde(rename = "$Type")]
        pub ty: DerefCell<'a, ty::Entity<'a>, ty::EntityPath>,
        #[serde(rename = "$Collection", skip_serializing_if = "bool_not")]
        pub collection: bool,
        #[serde(rename = "$Nullable", skip_serializing_if = "bool_not")]
        pub nullable: bool,
        //#[serde(rename = "$Partner")]
        // pub partner: DerefCell<'a, Navigation<'a>>,
        #[serde(rename = "$ContainsTarget")]
        pub contains_target: bool,
        // #[serde(rename = "$ReferentialConstraint")]
        // pub referential_constraint: Vec<(DerefCell<'a, , &'a Property<'a>, Document<'a>>,
        #[serde(rename = "$OnDelete")]
        pub on_delete: OnDeleteAction,
    }

    impl Named for Navigation<'_> {
        fn name(&self) -> &str {
            &self.name
        }
    }

    impl Default for Navigation<'_> {
        fn default() -> Self {
            Navigation {
                kind: String::from("NavigationProperty"),
                name: String::from(""),
                //FIXME this is not necessarily an entity
                parent: Parent::Entity(DerefCell::new()),
                //FIXME this is a dummy default that will panic. Find a better way to ergonomically
                //initialise a struct
                ty: DerefCell::with_state(ty::EntityPath("foo".to_string(), "bar".to_string())),
                collection: false,
                nullable: false,
                contains_target: false,
                on_delete: OnDeleteAction::None,
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq, Serialize)]
    pub enum OnDeleteAction {
        #[serde(rename = "Cascade")]
        Cascade,
        #[serde(rename = "None")]
        None,
        #[serde(rename = "SetNull")]
        SetNull,
        #[serde(rename = "SetDefault")]
        SetDefault,
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Action<'a> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(skip)]
    pub parent: DerefCell<'a, Schema<'a>>,
    #[serde(rename = "$Kind", default= "Action")]
    pub kind: String,
    #[serde(rename = "$IsBound")]
    pub is_bound: bool,
    #[serde(rename = "$EntitySetPath")]
    pub entity_set_path: String,
    #[serde(rename = "$Parameter")]
    pub parameter: Vec<Parameter<'a, Action<'a>>>,
    #[serde(rename = "$ReturnType", skip_serializing_if = "Option::is_none")]
    pub return_type: Option<ReturnType<'a, Action<'a>>>,
}

impl Named for Action<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

impl Default for Action<'_> {
    fn default() -> Self {
        Action {
            name: String::from(""),
            kind: String::from("Action"),
            parent: DerefCell::new(),
            is_bound: false,
            entity_set_path: String::from(""),
            parameter: vec![],
            return_type: Option::None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Function<'a> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(skip)]
    pub parent: DerefCell<'a, Schema<'a>>,
    #[serde(rename = "$Kind", default= "Action")]
    pub kind: String,
    #[serde(rename = "$IsBound")]
    pub is_bound: bool,
    #[serde(rename = "$EntitySetPath")]
    pub entity_set_path: String,
    #[serde(rename = "$Parameter")]
    pub parameters: Vec<Parameter<'a, Function<'a>>>,
    #[serde(rename = "$ReturnType")]
    pub return_type: ReturnType<'a, Function<'a>>,
    #[serde(rename = "$IsComposable")]
    pub is_composable: bool,
}

impl Named for Function<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

impl Default for Function<'_> {
    fn default() -> Self {
        Function {
            name: String::from(""),
            kind: String::from("Function"),
            parent: DerefCell::new(),
            is_bound: false,
            entity_set_path: String::from(""),
            parameters: vec![],
            return_type: ReturnType::default(),
            is_composable: false,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ReturnType<'a, Parent> {
    #[serde(skip)]
    pub parent: DerefCell<'a, Parent>,
    #[serde(rename = "$Type")]
    pub ty: ty::Ty<'a>,
    #[serde(rename = "$Collection")]
    pub collection: bool,
    #[serde(rename = "$Nullable")]
    pub nullable: bool,
    #[serde(rename = "$MaxLength")]
    pub max_length: u32,
    #[serde(rename = "$Unicode")]
    pub unicode: bool,
    #[serde(rename = "$Precision")]
    pub precision: u32,
    #[serde(rename = "$Scale")]
    pub scale: u32,
    #[serde(rename = "$SRID")]
    pub srid: u32,
}

impl<Parent> Default for ReturnType<'_, Parent> {
    fn default() -> Self {
        ReturnType {
            parent: DerefCell::new(),
            ty: ty::Ty::Primitive(ty::Primitive::Int64),
            collection: false,
            nullable: true,
            max_length: 0,
            unicode: false,
            precision: 0,
            scale: 0,
            srid: 0,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Parameter<'a, Parent> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(skip)]
    pub parent: DerefCell<'a, Parent>,
    #[serde(rename = "$Type")]
    pub ty: ty::Ty<'a>,
    #[serde(rename = "$Collection")]
    pub collection: bool,
    #[serde(rename = "$Nullable")]
    pub nullable: bool,
    #[serde(rename = "$MaxLength")]
    pub max_length: u32,
    #[serde(rename = "$Unicode")]
    pub unicode: bool,
    #[serde(rename = "$Precision")]
    pub precision: u32,
    #[serde(rename = "$Scale")]
    pub scale: u32,
    #[serde(rename = "$SRID")]
    pub srid: u32,
}

impl<Parent> Named for Parameter<'_, Parent> {
    fn name(&self) -> &str {
        &self.name
    }
}

impl<Parent> Default for Parameter<'_, Parent> {
    fn default() -> Self {
        Parameter {
            name: String::from(""),
            parent: DerefCell::new(),
            ty: ty::Ty::Primitive(ty::Primitive::Int64),
            collection: false,
            nullable: true,
            max_length: 0,
            unicode: false,
            precision: 0,
            scale: 0,
            srid: 0,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct EntityContainer<'a> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(rename = "$Kind", default= "EntityContainer")]
    pub kind: String,
    #[serde(skip)]
    pub parent: DerefCell<'a, Schema<'a>>,
    #[serde(rename = "$Extends", skip_serializing_if = "Option::is_none")]
    pub extends: Option<DerefCell<'a, EntityContainer<'a>>>,
    #[serde(flatten)]
    pub entity_sets: HashMap<Identifier, EntitySet<'a>>,
    #[serde(flatten)]
    pub singletons: HashMap<Identifier, Singleton<'a>>,
    #[serde(flatten)]
    pub action_imports: HashMap<Identifier, ActionImport<'a>>,
    #[serde(flatten)]
    pub function_imports: HashMap<Identifier, FunctionImport<'a>>,
}

impl Named for EntityContainer<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

impl Default for EntityContainer<'_> {
    fn default() -> Self {
        EntityContainer {
            name: String::from(""),
            kind: String::from("EntityContainer"),
            parent: DerefCell::new(),
            extends: None,
            entity_sets: HashMap::new(),
            singletons: HashMap::new(),
            action_imports: HashMap::new(),
            function_imports: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EntityContainerPath(pub String, pub String);

impl Serialize for EntityContainerPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
    {
        serializer.serialize_str(format!("{}.{}", self.0, self.1).as_str())
    }
}

impl<'a> Resolve<'a, EntityContainer<'a>> for EntityContainerPath {
    type Root = Document<'a>;

    fn resolve(&self, root: &'a Self::Root) -> &'a EntityContainer<'a> {
        root.schemas
            .get(&self.0)
            .unwrap()
            .entity_containers
            .get(&self.1)
            .unwrap()
    }
}

#[derive(Clone, Eq, PartialEq, Serialize)]
pub struct EntitySet<'a> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(rename = "$Collection")]
    pub collection: bool,
    #[serde(skip)]
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    #[serde(rename = "$$IncludeInServiceDocument", skip_serializing_if = "bool_is")]
    pub include_in_service_document: bool,
    #[serde(rename = "$Type")]
    pub ty: DerefCell<'a, ty::Entity<'a>, ty::EntityPath>,
}

impl Named for EntitySet<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

impl Default for EntitySet<'_> {
    fn default() -> Self {
        EntitySet {
            name: String::new(),
            collection: true,
            parent: DerefCell::new(),
            include_in_service_document: true,
            // FIXME find a better init method
            ty: DerefCell::with_state(ty::EntityPath("foo".to_string(), "bar".to_string())),
        }
    }
}

impl fmt::Debug for EntitySet<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "EntitySet({})", self.name)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EntitySetPath(pub String, pub String, pub String);

impl Serialize for EntitySetPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
    {
        serializer.serialize_str(format!("{}.{}", self.0, self.1).as_str())
    }
}

impl<'a> Resolve<'a, EntitySet<'a>> for EntitySetPath {
    type Root = Document<'a>;

    fn resolve(&self, root: &'a Self::Root) -> &'a EntitySet<'a> {
        root.schemas
            .get(&self.0)
            .unwrap()
            .entity_containers
            .get(&self.1)
            .unwrap()
            .entity_sets
            .get(&self.2)
            .unwrap()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Singleton<'a> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(skip)]
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    #[serde(rename = "$Type")]
    pub ty: ty::Ty<'a>,
    #[serde(rename = "$Nullable", skip_serializing_if = "bool_not")]
    pub nullable: bool,
}

impl Named for Singleton<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ActionImport<'a> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(skip)]
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    #[serde(rename = "$Action")]
    pub action: DerefCell<'a, Action<'a>, ActionPath>,
    #[serde(rename = "$EntitySet")]
    pub entity_set: DerefCell<'a, EntitySet<'a>, EntitySetPath>,
}

impl Named for ActionImport<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ActionPath(pub String, pub String);

impl Serialize for ActionPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
    {
        serializer.serialize_str(format!("{}.{}", self.0, self.1).as_str())
    }
}

impl<'a> Resolve<'a, Action<'a>> for ActionPath {
    type Root = Document<'a>;

    fn resolve(&self, root: &'a Self::Root) -> &'a Action<'a> {
        root.schemas
            .get(&self.0)
            .unwrap()
            .actions
            .get(&self.1)
            .unwrap()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct FunctionImport<'a> {
    #[serde(skip)]
    pub name: Identifier,
    #[serde(skip)]
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    #[serde(rename = "$Function")]
    pub function: DerefCell<'a, Function<'a>, FunctionPath>,
    #[serde(rename = "$EntitySet")]
    pub entity_set: DerefCell<'a, EntitySet<'a>, EntitySetPath>,
    #[serde(rename = "$IncludeInServiceDocument")]
    pub include_in_service_document: bool,
}

impl Named for FunctionImport<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionPath(pub String, pub String);

impl Serialize for FunctionPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
    {
        serializer.serialize_str(format!("{}.{}", self.0, self.1).as_str())
    }
}

impl<'a> Resolve<'a, Function<'a>> for FunctionPath {
    type Root = Document<'a>;

    fn resolve(&self, root: &'a Self::Root) -> &'a Function<'a> {
        root.schemas
            .get(&self.0)
            .unwrap()
            .functions
            .get(&self.1)
            .unwrap()
    }
}

pub fn map<T: Named + Clone>(v: &[T]) -> HashMap<String, T> {
    HashMap::from_iter(v.into_iter().cloned().map(|e| (e.name().to_string(), e)))
}
