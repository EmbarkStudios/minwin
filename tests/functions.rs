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

// Ensures we can emit functions that don't return. Only 4 of them in all of Win32, but still.
test!(
    no_return,
    [
        BindConfig::Minwin(Default::default()),
        BindConfig::Minwin(MinwinBindConfig {
            linking_style: LinkingStyle::RawDylib,
            ..Default::default()
        }),
        BindConfig::Bindgen,
    ],
    [
        "Windows.Win32.System.Diagnostics.Debug.FatalExit",
        "Windows.Win32.System.LibraryLoader.FreeLibraryAndExitThread",
        "Windows.Win32.System.Threading.ExitProcess",
        "Windows.Win32.System.Threading.ExitThread",
    ]
);