use minwin::bind;

/// Ensures we can generate more complex types
/// 1. Arch specific structs
/// 2. Records with alignment
/// 3. Records with packing
/// 4. Nested structs/unions
#[test]
fn complex() {
    let bindings = bind::bind(
        vec!["Windows.Win32.System.Diagnostics.Debug.RtlCaptureContext".to_owned()],
        Default::default(),
    )
    .expect("failed to bind");

    insta::assert_snapshot!(bindings.0);
}
