//FIXME
#![allow(dead_code)]
#![allow(unused_variables)]

use super::ast;
use super::schema;

#[derive(Debug,Clone)]
pub struct Query<'a> {
	select: Select<'a>,
	from: From<'a>,
	where_: Where<'a>,
	order_by: Option<OrderBy>,
	limit: Option<u64>,
	offset: Option<u64>,
	count: bool,
}

#[derive(Debug,Clone)]
struct Select<'a> {
	columns: Vec<&'a str>,
}

#[derive(Debug,Clone)]
struct From<'a> {
	table: &'a str,
}

#[derive(Debug,Clone)]
struct OrderBy{}

#[derive(Debug,Clone)]
struct Where<'a> {
	predicates: Vec<Predicate<'a>>,
}

#[derive(Debug,Clone)]
enum Predicate<'a> {
	Raw(&'a str),
	Exists(Query<'a>),
	NotExists(Query<'a>),
}

#[derive(Debug,Clone)]
enum SelectColumn {
	Plain,
	Query
}

// users/$filter=id lt 123/$filter=name eq 'foobar'/(12)
// 	/manager
// 	/applications/$filter=foo eq 23/(34)/
// 	name/$value
//
//
// SELECT
//   a.name
// FROM
//   application a
// WHERE
//   a.foo = 23 AND
//   a.id = 34 AND
//   EXISTS(SELECT 1 FROM users m WHERE a.owner = m.id AND
//     EXISTS(SELECT 1 FROM users u WHERE u.manager = m.id
//       u.id < 123 AND
//       u.name = 'foobar' AND
//       u.id = 12
//     )
//   )

#[derive(Debug)]
enum Kind<'a> {
	Primitive(&'a schema::kind::Primitive),
	Entity(&'a schema::kind::Entity),
	Complex(&'a schema::kind::Complex),
	Enumeration(&'a schema::kind::Enumeration),
}

#[derive(Debug)]
enum PathKind<'a> {
	None,
	Single(Kind<'a>),
	Collection(Kind<'a>),
}

pub fn compile_sql<'a>(uri: &'a ast::ODataURI, schema: &'a schema::Document) -> Query<'a> {
	match &uri.relative_uri {
		Some(ast::RelativeURI::Resource(r)) => {
			let mut result = Query {
				select: Select{columns: vec![]},
				from: From{table: "123"},
				where_: Where{predicates: vec![]},
				order_by: None,
				limit: None,
				offset: None,
				count: false
			};

			let mut cur_kind = PathKind::None;

			for segment in &r.segments {
				use ast::PathSegment::*;
				use PathKind::*;
				use Kind::*;

				println!("{:?}", segment);

				match (&cur_kind, segment) {
					(None, EntitySet(set)) => {
						result.from.table = &set.name;
						cur_kind = Collection(Entity(&set.kind));
					},
					(None, Singleton) => unimplemented!(),
					(None, Action) => unimplemented!(),
					(None, Function) => unimplemented!(),
					(None, Crossjoin) => unimplemented!(),
					(None, All) => unimplemented!(),
					(_, Cast) => unimplemented!(),
					(_, BoundOperation) => unimplemented!(),
					(Collection(_), Count) => result.count = true,
					(Collection(Entity(_)), Each) => unimplemented!(),
					(Collection(_), Filter(p)) => {
						result.where_.predicates.push(Predicate::Raw(p.name));
					},
					(Collection(Entity(kind)), KeyPredicate(k)) => {
						for value in &k.values {
							result.where_.predicates.push(Predicate::Raw("foobar"));
						}
						cur_kind = Single(Entity(kind))
					},
					(Single(Entity(_)), Property(p)) | (Single(Complex(_)), Property(p)) => {
						match p {
							schema::property::Property::Structural(s) => {
								cur_kind = match (&s.kind, s.collection) {
									(schema::property::Type::Primitive(kind), true) => Collection(Primitive(kind)),
									(schema::property::Type::Primitive(kind), false) => Single(Primitive(kind)),
									_ => unimplemented!(),
									// (schema::property::Type::Complex(kind), true) => Collection(Complex(kind)),
									// (schema::property::Type::Complex(kind), false) => Single(Complex(kind)),
									// (schema::property::Type::Enumeration(kind), true) => Collection(Enumeration(kind)),
									// (schema::property::Type::Enumeration(kind), false) => Single(Enumeration(kind)),
								};
								result.select.columns.push(&s.name)
							}
							schema::property::Property::Navigation(n) => {
								result.where_.predicates.push(Predicate::Raw("application.owner = users.id"));
								result = Query {
									select: Select{columns: vec![]},
									from: From{table: &n.kind.name},
									where_: Where{predicates: vec![Predicate::Exists(result)]},
									order_by: Option::None,
									limit: Option::None,
									offset: Option::None,
									count: false
								};

								cur_kind = match n.collection {
									true => Collection(Entity(&n.kind)),
									false => Single(Entity(&n.kind)),
								};
							}
						}
					},
					(Single(Entity(_)), Ref) | (Collection(Entity(_)), Ref) => unimplemented!(),
					(Single(Entity(_)), Value) | (Single(Primitive(_)), Value) => unimplemented!(),
					(Collection(Primitive(_)), OrdinalIndex(_)) | (Collection(Complex(_)), OrdinalIndex(_)) => unimplemented!(),
					a @ _ => {
						println!("{:#?}", a);
						panic!("Invalid sequence of path segments");
					}
				}
			}

			// match cur_kind {

			// }

			result
		},
		_ => unimplemented!(),
	}
}


// fn run_sql() {
// 	let query = conn.query(sql_string);
// 	match return_type {
// 		Collection => {
// 			for row in &query {
// 				for (i, column) in node.enumerate() {
// 					match column.type {
// 						String => serialize_str(row.get(i)),
// 						Object => serialize_entity(row.get(i)),
// 					}
//
// 				}
// 			}
//
// 		}
// 	}
// }

//
//
// SELECT ($select, $expand)
// FROM application
// WHERE $filter
// ORDER BY $orderby
// OFFSET $top $skip
//
//
