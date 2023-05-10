use minwin::bind;

/// Ensures we can generate more complex types
/// 1. Arch specific structs
/// 2. Records with alignment
/// 3. Records with packing
/// 4. Nested structs/unions
#[test]
fn complex() {
    let bo = bind::bind(
        ["Windows.Win32.System.Diagnostics.Debug.RtlCaptureContext"],
        Default::default(),
        bind::BindConfig::Minwin(Default::default()),
    )
    .expect("failed to bind");

    insta::assert_snapshot!(bo.bindings);

    let bo = bind::bind(
        ["Windows.Win32.System.Diagnostics.Debug.RtlCaptureContext"],
        Default::default(),
        bind::BindConfig::Bindgen,
    )
    .expect("failed to bind");

    insta::assert_snapshot!(bo.bindings);
}

/// Ensures we can add Copy + Clone implementations to specific records
#[test]
fn copy_clone() {
    let bo = bind::bind(
        [
            "Windows.Win32.System.Diagnostics.Debug.RtlCaptureContext",
            "Windows.Win32.System.Diagnostics.Debug.CONTEXT+Copy",
        ],
        Default::default(),
        bind::BindConfig::Minwin(Default::default()),
    )
    .expect("failed to bind");

    insta::assert_snapshot!(bo.bindings);
}
