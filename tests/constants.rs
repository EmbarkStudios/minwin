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
