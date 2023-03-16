#[test]
fn resolves() {
    let files = minwin::MetadataFiles::new().expect("failed to load metadata");
    let _resolver = minwin::Resolver::flatten(&files).expect("failed to resolve metadata");
}
