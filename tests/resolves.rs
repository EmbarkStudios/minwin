#[test]
fn resolves() {
    let files = minwin::MetadataFiles::new().expect("failed to load metadata");
    let resolver = minwin::Resolver::flatten(&files).expect("failed to resolve metadata");

    // resolver
    //     .namespaces
    //     .iter()
    //     .find_map(|(name, ns)| {
    //         ns.records.get(&"XSAVE_FORMAT".into()).map(|rec| {
    //             panic!("{name} {rec:?}");
    //         })
    //     })
    //     .expect("failed to locate");
}
