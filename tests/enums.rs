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
        BindConfig::Bindgen,
    ],
    [
        "Windows.Win32.Foundation.GENERIC_READ",
        "Windows.Win32.Foundation.GENERIC_WRITE",
        "Windows.Win32.Foundation.GENERIC_ALL",
    ]
);
