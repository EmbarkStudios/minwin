use minwin::bind;
use rstest::rstest;

/// Ensures we can generate string constants
#[rstest]
fn strings(
    #[values(
        bind::BindConfig::Minwin(Default::default()),
        // This is currently broken, see https://github.com/microsoft/windows-rs/issues/2499
        bind::BindConfig::Bindgen
    )]
    config: bind::BindConfig,
) {
    let name = config.to_string();
    let bo = bind::bind(
        [
            // utf-16
            "Windows.Win32.Devices.Enumeration.Pnp.ADDRESS_FAMILY_VALUE_NAME",
            // ansi
            "Windows.Win32.Media.Multimedia.JOY_CONFIGCHANGED_MSGSTRING",
        ],
        Default::default(),
        config,
    )
    .expect("failed to bind");

    insta::assert_snapshot!(format!("strings__{name}"), bo.bindings);
}
