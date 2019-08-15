use crate::schema::Document;

pub fn generate_csdl_json(doc: &Document) -> Result<String, serde_json::Error> {
    serde_json::to_string(doc)
}


#[cfg(test)]
mod tests {
    use crate::schema::map;
    use crate::csdl::generate_csdl_json;
    use crate::schema::{Document, EntityContainerPath, Schema, EntityContainer};
    use crate::derefcell::DerefCell;
    use crate::schema::ty::{Entity, Ty, Primitive};
    use crate::schema::property::{Structural};
    use crate::util::{retain_json_string, read_file_into_string};

    fn read_json_schema(path: &String) -> String {
        let mut contents = String::new();
        read_file_into_string(path, &mut contents).unwrap();
        retain_json_string(&mut contents);
        contents
    }

    #[test]
    fn csdl_simple_schema() {
        let namespace = "test.namespace".to_string();
        let doc = Document {
            version: "4.01".into(),
            service_root: "http://root.test/service/".into(),
            entity_container: DerefCell::with_state(EntityContainerPath(
                namespace.clone(),
                "test".to_string(),
            )),
            schemas: map(&[Schema {
                namespace: namespace.clone(),
                entity_containers: map(&[EntityContainer {
                    name: "test".into(),
                    ..EntityContainer::default()
                }]),
                ..Schema::default()
            }]),
            ..Document::default()
        };

        doc.link();

        let generated = generate_csdl_json(&doc).unwrap();
        let should_be = read_json_schema(&String::from("./tests/fixtures/csdl/simple_schema.json"));

        assert_eq!(generated, should_be);
    }

    #[test]
    fn csdl_entity_type() {
        let namespace = "test.namespace".to_string();
        let doc = Document {
            version: "4.01".into(),
            service_root: "http://root.test/service/".into(),
            entity_container: DerefCell::with_state(EntityContainerPath(
                namespace.clone(),
                "test".to_string(),
            )),
            schemas: map(&[Schema {
                namespace: namespace.clone(),
                entity_containers: map(&[EntityContainer {
                    name: "test".into(),
                    ..EntityContainer::default()
                }]),
                entity_types: map(&[
                    Entity {
                        name: "dummy_entity".into(),
                        open_type: true,
                        has_stream: true,
                        is_abstract: true,
                        structural_props: map(&[
                            Structural {
                                name: "id".into(),
                                ty: Ty::Primitive(Primitive::Guid),
                                collection: false,
                                nullable: false,
                                ..Structural::default()
                            }
                        ]),
                        ..Entity::default()
                    }
                ]),
                ..Schema::default()
            }]),
            ..Document::default()
        };

        doc.link();

        let generated = generate_csdl_json(&doc).unwrap();
        let should_be = read_json_schema(&String::from("./tests/fixtures/csdl/entity_types.json"));
        assert_eq!(generated, should_be);
    }
}
