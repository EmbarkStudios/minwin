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
