#[derive(Debug)]
pub enum RelativeURI<'a> {
	Batch(Option<Vec<QueryOption<'a>>>),
	Entity,
	Metadata,
	ResourcePath,
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
