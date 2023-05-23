mod utils;
use utils::*;

// Ensures we can emit interfaces and all of their dependencies
test_iface!(
    simple_interface,
    [
        BindConfig::Minwin(Default::default()),
        //BindConfig::Bindgen, doesn't work in 0.49
    ],
    [
    ],
    {
        "Windows.Win32.UI.Shell.IFileOpenDialog" => [],
    }
);
