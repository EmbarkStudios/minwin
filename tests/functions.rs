mod utils;
use utils::*;

// Ensures we can find and emit arch specific functions
test!(
    arches,
    [
        BindConfig::Minwin(Default::default()),
        BindConfig::Minwin(MinwinBindConfig {
            linking_style: LinkingStyle::RawDylib,
            ..Default::default()
        }),
        BindConfig::Bindgen,
    ],
    [
        "Windows.Win32.System.Search.SQLBindCol",
        "Windows.Win32.UI.WindowsAndMessaging.GetWindowLongPtrA",
        "Windows.Win32.UI.WindowsAndMessaging.GetWindowLongPtrW",
    ]
);
