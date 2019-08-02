//FIXME
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

use std::collections::HashMap;
use std::default::Default;
use std::fmt;
use std::iter::FromIterator;

use super::derefcell::DerefCell;
use super::derefcell::Resolve;

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Document<'a> {
    pub version: String,
    pub reference: Option<Vec<DerefCell<'a, Document<'a>>>>,
    pub entity_container: DerefCell<'a, EntityContainer<'a>, EntityContainerPath>,
    pub schemas: HashMap<String, Schema<'a>>,
    pub service_root: String,
}

impl<'a> Document<'a> {
    pub fn link(&'a self) {
        self.entity_container.link(self);
        for schema in self.schemas.values() {
            schema.parent.link(self);
            for entity_type in schema.entity_types.values() {
                entity_type.parent.link(&schema);
                if let Some(key) = &entity_type.key {
                    for _property in key.values() {
                        // property.link(self.resolve(property.state()));
                    }
                }
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Schema<'a> {
    pub namespace: Namespace,
    pub parent: DerefCell<'a, Document<'a>>,
    pub alias: Option<Identifier>,
    pub entity_types: HashMap<Identifier, ty::Entity<'a>>,
    pub complex_types: HashMap<Identifier, ty::Complex<'a>>,
    pub enum_types: HashMap<Identifier, ty::Enum<'a>>,
    pub type_definitions: HashMap<Identifier, ty::Definition<'a>>,
    pub actions: HashMap<Identifier, Action<'a>>,
    pub functions: HashMap<Identifier, Function<'a>>,
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

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum Ty<'a> {
        Primitive(Primitive),
        Entity(DerefCell<'a, Entity<'a>, EntityPath>),
        Complex(DerefCell<'a, Complex<'a>, ComplexPath>),
        Enum(DerefCell<'a, Enum<'a>, EnumPath>),
        Definition(DerefCell<'a, Definition<'a>, DefinitionPath>),
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

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum Primitive {
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

    #[derive(Debug, Eq, PartialEq)]
    pub enum Abstract {
        Primitive,
        Complex,
        Entity,
        Untyped,
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct Entity<'a> {
        pub name: String,
        pub parent: DerefCell<'a, super::Schema<'a>>,
        pub key: Option<HashMap<Identifier, DerefCell<'a, property::Property<'a>>>>,
        pub base_type: Option<DerefCell<'a, Entity<'a>, EntityPath>>,
        pub is_abstract: bool,
        pub open_type: bool,
        pub has_stream: bool,
        pub structural_props: HashMap<Identifier, property::Structural<'a>>,
        pub navigation_props: HashMap<Identifier, property::Navigation<'a>>,
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

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct Complex<'a> {
        pub name: String,
        pub parent: DerefCell<'a, super::Schema<'a>>,
        pub base_type: Option<DerefCell<'a, Complex<'a>, ComplexPath>>,
        pub is_abstract: bool,
        pub open_type: bool,
        pub structural_props: HashMap<Identifier, property::Structural<'a>>,
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

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct Enum<'a> {
        pub name: String,
        pub parent: DerefCell<'a, super::Schema<'a>>,
        pub underlying_type: EnumBase,
        pub is_flags: bool,
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
                parent: DerefCell::new(),
                underlying_type: EnumBase::Int16,
                is_flags: false,
                members: HashMap::new(),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct EnumPath(pub String, pub String);

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

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum EnumBase {
        Byte,
        SByte,
        Int16,
        Int32,
        Int64,
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct Definition<'a> {
        pub name: Identifier,
        pub parent: DerefCell<'a, super::Schema<'a>>,
        pub underlying_type: Primitive,
        pub max_length: u32,
        pub unicode: bool,
        pub precision: u32,
        pub scale: u32,
        pub srid: u32,
    }

    impl Named for Definition<'_> {
        fn name(&self) -> &str {
            &self.name
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
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

    #[derive(Clone, Debug, Eq, PartialEq)]
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

    #[derive(Clone, Debug, Eq, PartialEq)]
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

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct Structural<'a> {
        pub name: Identifier,
        pub parent: Parent<'a>,
        pub ty: ty::Ty<'a>,
        pub collection: bool,
        pub nullable: bool,
        pub max_length: u32,
        pub unicode: bool,
        pub precision: u32,
        pub scale: u32,
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

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct Navigation<'a> {
        pub name: Identifier,
        pub parent: Parent<'a>,
        pub ty: DerefCell<'a, ty::Entity<'a>, ty::EntityPath>,
        pub collection: bool,
        pub nullable: bool,
        // pub partner: DerefCell<'a, Navigation<'a>>,
        pub contains_target: bool,
        // pub referential_constraint: Vec<(DerefCell<'a, , &'a Property<'a>, Document<'a>>,
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

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum OnDeleteAction {
        Cascade,
        None,
        SetNull,
        SetDefault,
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Action<'a> {
    pub name: Identifier,
    pub parent: DerefCell<'a, Schema<'a>>,
    pub is_bound: bool,
    pub entity_set_path: String,
    pub parameter: Vec<Parameter<'a, Action<'a>>>,
    pub return_type: Option<ReturnType<'a, Action<'a>>>,
}

impl Named for Action<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function<'a> {
    pub name: Identifier,
    pub parent: DerefCell<'a, Schema<'a>>,
    pub is_bound: bool,
    pub entity_set_path: String,
    pub parameter: Vec<Parameter<'a, Function<'a>>>,
    pub return_type: ReturnType<'a, Function<'a>>,
    pub is_composable: bool,
}

impl Named for Function<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReturnType<'a, Parent> {
    pub parent: DerefCell<'a, Parent>,
    pub ty: ty::Ty<'a>,
    pub collection: bool,
    pub nullable: bool,
    pub max_length: u32,
    pub unicode: bool,
    pub precision: u32,
    pub scale: u32,
    pub srid: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Parameter<'a, Parent> {
    pub name: Identifier,
    pub parent: DerefCell<'a, Parent>,
    pub ty: ty::Ty<'a>,
    pub collection: bool,
    pub nullable: bool,
    pub max_length: u32,
    pub unicode: bool,
    pub precision: u32,
    pub scale: u32,
    pub srid: u32,
}

impl<Parent> Named for Parameter<'_, Parent> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EntityContainer<'a> {
    pub name: Identifier,
    pub parent: DerefCell<'a, Schema<'a>>,
    pub extends: Option<DerefCell<'a, EntityContainer<'a>>>,
    pub entity_sets: HashMap<Identifier, EntitySet<'a>>,
    pub singletons: HashMap<Identifier, Singleton<'a>>,
    pub action_imports: HashMap<Identifier, ActionImport<'a>>,
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

#[derive(Clone, Eq, PartialEq)]
pub struct EntitySet<'a> {
    pub name: Identifier,
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    pub include_in_service_document: bool,
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
pub struct Singleton<'a> {
    pub name: Identifier,
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    pub ty: ty::Ty<'a>,
    pub nullable: bool,
}

impl Named for Singleton<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ActionImport<'a> {
    pub name: Identifier,
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    pub action: DerefCell<'a, Action<'a>>,
    pub entity_set: DerefCell<'a, EntitySet<'a>>,
}

impl Named for ActionImport<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionImport<'a> {
    pub name: Identifier,
    pub parent: DerefCell<'a, EntityContainer<'a>>,
    pub function: DerefCell<'a, Function<'a>>,
    pub entity_set: DerefCell<'a, EntitySet<'a>>,
    pub include_in_service_document: bool,
}

impl Named for FunctionImport<'_> {
    fn name(&self) -> &str {
        &self.name
    }
}

pub fn map<T: Named + Clone>(v: &[T]) -> HashMap<String, T> {
    HashMap::from_iter(v.into_iter().cloned().map(|e| (e.name().to_string(), e)))
}
