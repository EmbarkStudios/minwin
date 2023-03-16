// use tracing_subscriber::{prelude::__tracing_subscriber_SubscriberExt, Layer};
//     let layer = tracing_subscriber::fmt::layer()
//         .with_test_writer()
//         .with_filter(tracing::metadata::LevelFilter::from_level(
//             tracing::metadata::Level::TRACE,
//         ));
//     let reg = tracing_subscriber::Registry::default().with(layer);
//     let dis = tracing::Dispatch::new(reg);
//     //let _guard = tracing::dispatcher::set_global_default(dis);

#[test]
fn generates_simple() {
    let mut parser = minwin::Parser::default();
    parser.add_file("tests/data/parking_lot.rs");
    let parsed = parser.parse();

    let md = minwin::MetadataFiles::new().expect("failed to gather metadata files");
    let resolved = minwin::Resolver::flatten(&md).expect("failed to resolve metadata");

    let ts = minwin::generate(&resolved, parsed[0].iter_bind_modules().next().unwrap())
        .expect("failed to generate");

    insta::assert_snapshot!(ts.to_string());
}

#[test]
fn generates_complex() {
    let mut parser = minwin::Parser::default();
    parser.add_file("tests/data/md_writer.rs");
    let parsed = parser.parse();

    let md = minwin::MetadataFiles::new().expect("failed to gather metadata files");
    let resolved = minwin::Resolver::flatten(&md).expect("failed to resolve metadata");

    let ts = minwin::generate(&resolved, parsed[0].iter_bind_modules().next().unwrap())
        .expect("failed to generate");

    insta::assert_snapshot!(ts.to_string());
}
