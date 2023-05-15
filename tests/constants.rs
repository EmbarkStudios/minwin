mod utils;
use utils::*;

test!(
    regular,
    [
        BindConfig::Minwin(Default::default()),
        BindConfig::Minwin(MinwinBindConfig {
            use_rust_casing: true,
            ..Default::default()
        }),
        BindConfig::Bindgen,
    ],
    [
        "Windows.Win32.Graphics.OpenGL.GLU_BEGIN",
        "Windows.Win32.Graphics.OpenGL.GLU_CCW",
        "Windows.Win32.Graphics.OpenGL.GL_ADD",
        "Windows.Win32.Graphics.OpenGL.GL_ALL_ATTRIB_BITS",
    ]
);

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

// Ensures we can generate constants that need casting to the appropriate type
// currently broken for bindgen <https://github.com/microsoft/windows-rs/issues/2504>
test!(
    casting,
    [BindConfig::Minwin(Default::default()), BindConfig::Bindgen,],
    [
        "Windows.Win32.UI.WindowsAndMessaging.IDC_SIZENESW",
        "Windows.Win32.UI.WindowsAndMessaging.IDC_SIZENS",
        "Windows.Win32.UI.WindowsAndMessaging.IDC_SIZENWSE",
        "Windows.Win32.UI.WindowsAndMessaging.IDC_SIZEWE",
        "Windows.Win32.UI.WindowsAndMessaging.IDC_UPARROW",
    ]
);
