use std::fmt::Write;

#[test]
fn parses_simple() {
    let mut parser = minwin::Parser::default();
    parser.add_file("tests/data/parking_lot.rs");

    let parsed = parser.parse();

    let mut actual = String::new();

    for pf in parsed {
        writeln!(&mut actual, "{}", pf.path).unwrap();

        let mut module = None;

        for i in pf.iter_binds() {
            if Some(&i.module.ident) != module {
                writeln!(&mut actual, "mod {}", i.module.ident).unwrap();
                module = Some(&i.module.ident);
            }
            writeln!(&mut actual, "    {:?} {}", i.kind, i.ident).unwrap();
        }
    }

    insta::assert_snapshot!(actual);
}
