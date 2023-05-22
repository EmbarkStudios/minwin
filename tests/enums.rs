mod utils;
use utils::*;

// Ensures we can emit some but not all of a simple enum's values
test!(
    simple,
    [
        BindConfig::Minwin(Default::default()),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            ..Default::default()
        }),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            use_rust_casing: true,
            ..Default::default()
        }),
        BindConfig::Bindgen,
    ],
    [
        "Windows.Win32.Foundation.GENERIC_READ",
        "Windows.Win32.Foundation.GENERIC_WRITE",
        "Windows.Win32.Foundation.GENERIC_ALL",
        "Windows.Win32.Storage.FileSystem.FILE_TYPE_DISK",
        "Windows.Win32.Storage.FileSystem.GetFileType",
        "Windows.Win32.Storage.FileSystem.ClfsMgmtLogWriteNotification",
        "Windows.Win32.Storage.FileSystem.ReadLogNotification",
    ]
);

// Ensures we can emit enums that use core types instead of a typedef
test!(
    core,
    [
        BindConfig::Minwin(Default::default()),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            ..Default::default()
        }),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            use_rust_casing: true,
            ..Default::default()
        }),
        BindConfig::Bindgen,
    ],
    ["Windows.Win32.Foundation.S_OK"]
);

// Ensures we can emit the enum type alias, even if no actual enum variants
// are requested
test!(
    always_emits_alias,
    [
        BindConfig::Minwin(Default::default()),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            ..Default::default()
        }),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            use_rust_casing: true,
            ..Default::default()
        }),
        BindConfig::Bindgen,
    ],
    ["Windows.Win32.UI.Input.Pointer.GetPointerTouchInfo"]
);

// Ensures that enums that clash with handle types are supported
test!(
    handle_and_enum,
    [
        BindConfig::Minwin(Default::default()),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            ..Default::default()
        }),
        BindConfig::Minwin(MinwinBindConfig {
            enum_style: EnumStyle::Minwin,
            use_rust_casing: true,
            ..Default::default()
        }),
        BindConfig::Bindgen,
    ],
    [
        "Windows.Win32.UI.WindowsAndMessaging.SetWindowPos",
        "Windows.Win32.UI.WindowsAndMessaging.GetMessageW",
        "Windows.Win32.UI.WindowsAndMessaging.HWND_NOTOPMOST",
    ]
);
