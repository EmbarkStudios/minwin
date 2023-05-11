mod utils;
use utils::*;

// Ensures we can generate string constants
test!(
    strings,
    [BindConfig::Minwin(Default::default()), BindConfig::Bindgen,],
    [
        // utf-16
        "Windows.Win32.Devices.Enumeration.Pnp.ADDRESS_FAMILY_VALUE_NAME",
        // ansi
        "Windows.Win32.Media.Multimedia.JOY_CONFIGCHANGED_MSGSTRING",
    ]
);
