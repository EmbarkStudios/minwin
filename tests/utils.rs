pub use minwin::bind::*;

#[macro_export]
macro_rules! test {
    ($name:ident, [$($config:expr),*$(,)?], [$($item:literal),*$(,)?]) => {
        #[rstest::rstest]
        fn $name(
            #[values($($config),*)]
            config: BindConfig,
        ) {
            let name = config.to_string();
            let bo = bind(
                [
                    $($item),*
                ],
                Default::default(),
                config,
            )
            .expect("failed to bind");

            insta::assert_snapshot!(format!("{}__{name}", stringify!($name)), bo.bindings);
        }
    }
}

#[macro_export]
macro_rules! test_iface {
    ($name:ident, [$($config:expr),*$(,)?], [$($item:literal),*$(,)?], { $($iface:literal => [$($method:literal),*$(,)?]),*$(,)? }) => {
        #[rstest::rstest]
        fn $name(
            #[values($($config),*)]
            config: BindConfig,
        ) {
            let name = config.to_string();

            let mut ifaces = std::collections::BTreeMap::new();
            $(
                ifaces.insert($iface.to_owned(), [$($method),*].iter().map(|s: &&'static str| (*s).to_owned()).collect());
            )*

            let items: Vec<_> = [
                $($item),*
            ].iter().map(|s: &&'static str| (*s).to_owned()).collect();

            let bo = bind(
                items,
                ifaces,
                config,
            )
            .expect("failed to bind");

            insta::assert_snapshot!(format!("{}__{name}", stringify!($name)), bo.bindings);
        }
    }
}
