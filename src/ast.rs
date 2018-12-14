// enum RelativeURI {
// 	Batch(Option<BatchOptions>),
// 	Entity(),
// 	Metadata(Option<MetadataOption>, Option<Context>),
// 	ResourcePath(String, Option<QueryOptions>),
// }

#[derive(Debug)]
pub enum RelativeURI {
	Batch,
	Entity,
	Metadata,
	ResourcePath,
}
