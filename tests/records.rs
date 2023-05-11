mod utils;
use utils::*;

// Ensures we can generate more complex types
// 1. Arch specific structs
// 2. Records with alignment
// 3. Records with packing
// 4. Nested structs/unions
test!(
    complex,
    [BindConfig::Minwin(Default::default()), BindConfig::Bindgen],
    ["Windows.Win32.System.Diagnostics.Debug.RtlCaptureContext"]
);

// Ensures we can add Copy + Clone implementations to specific records
test!(
    copy_clone,
    [BindConfig::Minwin(Default::default()), BindConfig::Bindgen],
    [
        "Windows.Win32.System.Diagnostics.Debug.RtlCaptureContext",
        "Windows.Win32.System.Diagnostics.Debug.CONTEXT+Copy",
    ]
);

// Ensures we can handle the GUID core type
test!(
    guid,
    [BindConfig::Minwin(Default::default()), BindConfig::Bindgen],
    ["Windows.Win32.System.Com.CoCreateGuid",]
);
