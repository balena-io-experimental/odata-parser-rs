//FIXME
#![allow(dead_code)]

use std::str::FromStr;

use super::schema;
use uuid::Uuid;

#[derive(Debug,Clone)]
pub struct ODataURI<'a> {
	pub service_root: &'a str,
	pub relative_uri: Option<RelativeURI<'a>>,
}

#[derive(Debug,Clone)]
pub struct ServiceRoot<'a> {
	pub data: &'a str,
}

#[derive(Debug,Clone)]
pub enum RelativeURI<'a> {
	Batch(Option<Vec<QueryOption<'a>>>),
	Entity,
	Metadata,
	Resource(ResourcePath<'a>),
}

#[derive(Debug,Clone)]
pub struct ResourcePath<'a> {
	pub segments: Vec<PathSegment<'a>>,
	pub options: Option<Vec<QueryOption<'a>>>,
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
	Compute,
	DeltaToken,
	Expand,
	Filter(Expr),
	Id,
	InlineCount,
	OrderBy,
	SchemaVersion,
	Search,
	Select,
	Skip,
	SkipToken,
	Top,
	Index,
	Alias,
	Name,
	Format(FormatKind<'a>),
	Custom(&'a str),
}

#[derive(Debug,Clone)]
pub enum UnOp {
	/// The `not` operator for logical inversion
	Not,
	/// The `-` operator for negation
	Neg,
}

impl UnOp {
	pub fn precedence(&self) -> u8 {
		match self {
			UnOp::Not => 6,
			UnOp::Neg => 6,
		}
	}
}

#[derive(Debug,Clone)]
pub enum BinOp {
	/// The `add` operator (addition)
	Add,
	/// The `sub` operator (subtraction)
	Sub,
	/// The `mul` operator (multiplication)
	Mul,
	/// The `div` operator (division)
	Div,
	/// The `divby` operator (decimal division)
	DivBy,
	/// The `mod` operator (modulus)
	Mod,
	/// The `and` operator (logical and)
	And,
	/// The `or` operator (logical or)
	Or,
	/// The `eq` operator (equality)
	Eq,
	/// The `lt` operator (less than)
	Lt,
	/// The `le` operator (less than or equal to)
	Le,
	/// The `ne` operator (not equal to)
	Ne,
	/// The `ge` operator (greater than or equal to)
	Ge,
	/// The `gt` operator (greater than)
	Gt,
	/// The `has` operator (enumeration flags)
	Has,
	/// The `in` operator (is member of)
	In,
}

impl BinOp {
	pub fn precedence(&self) -> u8 {
		match self {
			BinOp::Or => 0,
			BinOp::And => 1,
			BinOp::Eq => 2,
			BinOp::Ne => 2,
			BinOp::Lt => 3,
			BinOp::Le => 3,
			BinOp::Ge => 3,
			BinOp::Gt => 3,
			BinOp::Add => 4,
			BinOp::Sub => 4,
			BinOp::Mul => 5,
			BinOp::Div => 5,
			BinOp::DivBy => 5,
			BinOp::Mod => 5,
			BinOp::Has => 7,
			BinOp::In => 7,
		}
	}
}

#[derive(Debug,Clone)]
pub enum IntTy {
	U8,
	I8,
	I16,
	I32,
	I64,
}

#[derive(Debug,Clone)]
pub enum FloatTy {
	F32,
	F64,
}

#[derive(Debug,Clone)]
pub enum Lit {
	Null,
	Binary(String),
	Boolean(bool),
	Date(i16, u8, u8),
	DateTimeOffset(i16, u8, u8, u8, u8),
	Decimal,
	Float(f64, FloatTy),
	Duration,
	Enum,
	Guid(Uuid),
	Int(i64, IntTy),
	Str(String),
	TimeOfDay,
	GeographyPoint,
	GeographyLineString,
	GeographyPolygon,
	GeographyMultiPoint,
	GeographyMultiLineString,
	GeographyMultiPolygon,
	GeographyCollection,
	GeometryPoint,
	GeometryLineString,
	GeometryPolygon,
	GeometryMultiPoint,
	GeometryMultiLineString,
	GeometryMultiPolygon,
	GeometryCollection,
	Unimplemented,
}

#[derive(Debug,Clone)]
pub enum Expr {
	Call(Box<Expr>, Vec<Expr>),
	Lit(Lit),
	Binary(BinOp, Box<Expr>, Box<Expr>),
	Unary(UnOp, Box<Expr>),
	List(Vec<Expr>),
	// Cast,
	// IsOf,
	MethodCall(Method, Vec<Expr>),
	// Root,
	// JSON,
	// Member,
}

#[derive(Debug,Clone)]
pub enum Method {
	Substring,
	Concat,
	Contains,
	EndsWith,
	IndexOf,
	StartsWith,
	GeoDistance,
	GeoIntersects,
	HasSubset,
	HasSubsequence,
	Length,
	ToLower,
	ToUpper,
	Trim,
	Year,
	Month,
	Day,
	Hour,
	Minute,
	Second,
	FractionalSeconds,
	TotalSeconds,
	Date,
	Time,
	TotalOffsetMinutes,
	Round,
	Floor,
	Ceiling,
	GeoLength,
	MinDatetime,
	MaxDatetime,
	Now,
}

pub struct MethodParseError(());

impl Method {
	pub fn arity(&self) -> (usize, usize) {
		match self {
			Method::Substring => (2, 3),
			Method::Concat => (2, 2),
			Method::Contains => (2, 2),
			Method::EndsWith => (2, 2),
			Method::IndexOf => (2, 2),
			Method::StartsWith => (2, 2),
			Method::GeoDistance => (2, 2),
			Method::GeoIntersects => (2, 2),
			Method::HasSubset => (2, 2),
			Method::HasSubsequence => (2, 2),
			Method::Length => (1, 1),
			Method::ToLower => (1, 1),
			Method::ToUpper => (1, 1),
			Method::Trim => (1, 1),
			Method::Year => (1, 1),
			Method::Month => (1, 1),
			Method::Day => (1, 1),
			Method::Hour => (1, 1),
			Method::Minute => (1, 1),
			Method::Second => (1, 1),
			Method::FractionalSeconds => (1, 1),
			Method::TotalSeconds => (1, 1),
			Method::Date => (1, 1),
			Method::Time => (1, 1),
			Method::TotalOffsetMinutes => (1, 1),
			Method::Round => (1, 1),
			Method::Floor => (1, 1),
			Method::Ceiling => (1, 1),
			Method::GeoLength => (1, 1),
			Method::MinDatetime => (0, 0),
			Method::MaxDatetime => (0, 0),
			Method::Now => (0, 0),
		}
	}
}

impl FromStr for Method {
    type Err = MethodParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"substring" => Ok(Method::Substring),
			"concat" => Ok(Method::Concat),
			"contains" => Ok(Method::Contains),
			"endswith" => Ok(Method::EndsWith),
			"indexof" => Ok(Method::IndexOf),
			"startswith" => Ok(Method::StartsWith),
			"geo.distance" => Ok(Method::GeoDistance),
			"geo.intersects" => Ok(Method::GeoIntersects),
			"hassubset" => Ok(Method::HasSubset),
			"hassubsequence" => Ok(Method::HasSubsequence),
			"length" => Ok(Method::Length),
			"tolower" => Ok(Method::ToLower),
			"toupper" => Ok(Method::ToUpper),
			"trim" => Ok(Method::Trim),
			"year" => Ok(Method::Year),
			"month" => Ok(Method::Month),
			"day" => Ok(Method::Day),
			"hour" => Ok(Method::Hour),
			"minute" => Ok(Method::Minute),
			"second" => Ok(Method::Second),
			"fractionalseconds" => Ok(Method::FractionalSeconds),
			"totalseconds" => Ok(Method::TotalSeconds),
			"date" => Ok(Method::Date),
			"time" => Ok(Method::Time),
			"totaloffsetminutes" => Ok(Method::TotalOffsetMinutes),
			"round" => Ok(Method::Round),
			"floor" => Ok(Method::Floor),
			"ceiling" => Ok(Method::Ceiling),
			"geo.length" => Ok(Method::GeoLength),
			"mindatetime" => Ok(Method::MinDatetime),
			"maxdatetime" => Ok(Method::MaxDatetime),
			"now" => Ok(Method::Now),
			_ => Err(MethodParseError(())),
		}
	}
}
