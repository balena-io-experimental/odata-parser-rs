use super::schema;

#[derive(Debug)]
pub struct ODataURI<'a> {
	pub service_root: ServiceRoot<'a>,
	pub relative_uri: RelativeURI<'a>,
}

#[derive(Debug)]
pub struct ServiceRoot<'a> {
	pub data: &'a str,
}

#[derive(Debug)]
pub enum RelativeURI<'a> {
	None,
	Batch(Option<Vec<QueryOption<'a>>>),
	Entity,
	Metadata,
	Resource(ResourcePath<'a>),
}

#[derive(Debug)]
pub enum ResourcePath<'a> {
	Unimplemented(&'a str),
	EntitySet(&'a schema::EntitySet),
}

impl<'a> From<&'a str> for ResourcePath<'a> {
	fn from(s: &'a str) -> Self {
		ResourcePath::Unimplemented(s)
	}
}


#[derive(Debug)]
pub enum FormatKind<'a> {
	JSON,
	Atom,
	XML,
	Custom(&'a str),
}

#[derive(Debug)]
pub enum QueryOption<'a> {
	Format(FormatKind<'a>),
	Custom(&'a str),
}
