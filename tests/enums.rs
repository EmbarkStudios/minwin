use minwin::bind;
use rstest::rstest;

/// Ensures we can emit some but not all of a simple enum's values
#[rstest]
fn simple(
    #[values(
        bind::BindConfig::Minwin(Default::default()),
        bind::BindConfig::Minwin(bind::MinwinBindConfig {
            enum_style: bind::EnumStyle::Minwin,
            ..Default::default()
        }),
        bind::BindConfig::Bindgen
    )]
    config: bind::BindConfig,
) {
    let name = config.to_string();
    let bo = bind::bind(
        [
            "Windows.Win32.Foundation.GENERIC_READ",
            "Windows.Win32.Foundation.GENERIC_WRITE",
            "Windows.Win32.Foundation.GENERIC_ALL",
        ],
        Default::default(),
        config,
    )
    .expect("failed to bind");

    insta::assert_snapshot!(format!("simple__{name}"), bo.bindings);
}
