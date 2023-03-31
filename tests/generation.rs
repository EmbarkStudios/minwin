// use tracing_subscriber::{prelude::__tracing_subscriber_SubscriberExt, Layer};
//     let layer = tracing_subscriber::fmt::layer()
//         .with_test_writer()
//         .with_filter(tracing::metadata::LevelFilter::from_level(
//             tracing::metadata::Level::TRACE,
//         ));
//     let reg = tracing_subscriber::Registry::default().with(layer);
//     let dis = tracing::Dispatch::new(reg);
//     //let _guard = tracing::dispatcher::set_global_default(dis);

use anyhow::Context as _;

fn generate(input: &str, format: bool, convert_case: bool) {
    let mut parser = minwin::Parser::default();
    parser.add_file(format!("tests/data/{input}.rs"));
    let mut parsed = parser.parse().pop().unwrap();

    let mut hints = minwin::Hints::default();
    parsed.gather_hints(&mut hints);

    let md = minwin::MetadataFiles::new().expect("failed to gather metadata files");
    let resolved = minwin::Resolver::flatten(&md, hints).expect("failed to resolve metadata");

    let generated = parsed
        .iter_bind_modules()
        .enumerate()
        .map(|(i, m)| {
            let ts = minwin::generate(&resolved, m, convert_case)
                .with_context(|| format!("{}", m.ident))?;
            Ok((i, ts))
        })
        .collect::<anyhow::Result<Vec<_>>>()
        .expect("failed to generate modules");

    for (index, ts) in generated {
        parsed
            .replace_module(index, ts)
            .expect("failed to replace {index}");
    }

    let generated = parsed.generate(format).expect("failed to generate");

    insta::assert_snapshot!(format!("{input}_{format}"), generated);
}

/// Ensures we can generate function pointers for regular functions for use
/// with eg. `GetProcAddress`
#[test]
fn delegates_for_functions() {
    generate("parking_lot", true, false);
}

/// Ensures we can generate more complex types
/// 1. Arch specific structs
/// 2. Records with alignment
/// 3. Records with packing
/// 4. Nested structs/unions
#[test]
fn complex() {
    generate("md_writer", true, false);
}

/// While the windows metadata professes to encourage enums instead of plain
/// integer constants, there...aren't that many. But just check we can emit the
/// basics
#[test]
fn enums() {
    generate("enums", true, false);
}

/// Ensures we can emit function pointers for actual function pointers
#[test]
fn delegates() {
    generate("delegates", true, false);
}
