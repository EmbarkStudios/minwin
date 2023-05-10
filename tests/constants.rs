use minwin::bind;

/// Ensures we can generate string constants
#[test]
fn string_constants() {
    let bo = bind::bind(
        [
            // utf-16
            "Windows.Win32.Devices.Enumeration.Pnp.ADDRESS_FAMILY_VALUE_NAME",
            // ansi
            "Windows.Win32.Media.Multimedia.JOY_CONFIGCHANGED_MSGSTRING",
        ],
        Default::default(),
        bind::BindConfig::Minwin(Default::default()),
    )
    .expect("failed to bind");

    insta::assert_snapshot!(bo.bindings);

    let bo = bind::bind(
        [
            // utf-16
            "Windows.Win32.Devices.Enumeration.Pnp.ADDRESS_FAMILY_VALUE_NAME",
            // ansi
            "Windows.Win32.Media.Multimedia.JOY_CONFIGCHANGED_MSGSTRING",
        ],
        Default::default(),
        bind::BindConfig::Bindgen,
    )
    .expect("failed to bind");

    insta::assert_snapshot!(bo.bindings);
}
