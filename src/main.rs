use anyhow::Context as _;
use camino::Utf8PathBuf as PathBuf;
use clap::Parser;

#[derive(Parser, Clone)]
struct Generate {
    /// Disable formatting via `rustfmt`
    #[clap(long)]
    no_fmt: bool,
    /// The specific package to find and generate bindings for
    ///
    /// If not specified and used within a workspace, bindings are generated
    /// for all crates unless `files` are also not specified
    #[clap(short, long, group = "input")]
    package: Option<String>,
    /// The file to emit the bindings to, defaults to stdout if not specified
    #[clap(group = "input")]
    files: Vec<PathBuf>,
}

#[derive(Parser, Clone)]
struct Bind {
    /// Path to the toml file detailing the items to bind
    path: PathBuf,
}

#[derive(Parser, Clone)]
struct Search {
    items: Vec<String>,
}

#[derive(clap::Subcommand, Clone)]
enum SubCmd {
    Generate(Generate),
    Bind(Bind),
    Search(Search),
}

#[derive(Parser)]
struct Cmd {
    #[clap(subcommand)]
    cmd: SubCmd,
}

fn main() -> anyhow::Result<()> {
    use tracing_subscriber::prelude::*;

    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let opts = Cmd::parse();

    match opts.cmd {
        SubCmd::Generate(opts) => {
            generate(opts)?;
        }
        SubCmd::Bind(opts) => {
            bind(opts)?;
        }
        SubCmd::Search(opts) => {
            search(opts)?;
        }
    }

    Ok(())
}

fn generate(opts: Generate) -> anyhow::Result<()> {
    let mut parser = minwin::Parser::default();

    if !opts.files.is_empty() {
        for file in opts.files {
            parser.add_file(file);
        }
    } else {
        let cmd = cargo_metadata::MetadataCommand::new();
        let cm = cmd
            .exec()
            .context("failed to gather metadata for workspace")?;

        if let Some(package) = opts.package {
            let krate = cm
                .packages
                .iter()
                .find(|pkg| pkg.name == package)
                .with_context(|| format!("unable to locate crate '{package}'"))?;
            parser.add_crate(krate);
        } else {
            parser.add_workspace(&cm);
        }
    }

    let mut parsed = parser.parse();

    let mut hints = minwin::Hints::default();

    for bf in &parsed {
        bf.gather_hints(&mut hints);
    }

    let md = minwin::MetadataFiles::new().context("failed to gather metadata files")?;
    let resolver = minwin::Resolver::flatten(&md, hints).context("failed to resolve metadata")?;

    for pf in &mut parsed {
        let genned = pf
            .iter_bind_modules()
            .enumerate()
            .map(|(i, m)| {
                let ts = minwin::generate(&resolver, m, false)
                    .with_context(|| format!("{}", m.ident))?;
                Ok((i, ts))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        for (i, ts) in genned {
            pf.replace_module(i, ts)?;
        }
    }

    let run_rustfmt = !opts.no_fmt;

    for pf in parsed {
        pf.replace(run_rustfmt)
            .with_context(|| format!("failed to generate bindings in '{}'", pf.path))?;
    }

    Ok(())
}

use windows_metadata::reader as wmr;

fn search(opts: Search) -> anyhow::Result<()> {
    let files = &wmr::File::with_default(&[]).unwrap();
    let reader = &wmr::Reader::new(files);

    use windows_bindgen::Disambiguate as Dis;

    for (item, dis) in minwin::qualify_items(reader, opts.items) {
        println!(
            "{}{}",
            match dis {
                None => "",
                Some(Dis::Constant) => "(constant) ",
                Some(Dis::Function) => "(function) ",
                Some(Dis::Record) => "(record) ",
            },
            nu_ansi_term::Color::Blue.paint(item)
        );
    }

    Ok(())
}

#[derive(serde::Deserialize)]
struct Config {
    output: PathBuf,
    binds: Vec<String>,
    interfaces: BTreeMap<String, BTreeSet<String>>,
}

impl Config {
    pub fn load(path: &camino::Utf8Path) -> anyhow::Result<Self> {
        let cfg_file =
            std::fs::read_to_string(path).with_context(|| format!("failed to read '{path}'"))?;

        toml::from_str(&cfg_file).with_context(|| format!("failed to parse config from '{path}'"))
    }
}

fn bind(opts: Bind) -> anyhow::Result<()> {
    let cfg = Config::load(&opts.path)?;

    let (bound, items) = minwin::bind(cfg.binds, is_sys)?;

    let out_path = if cfg.output.as_str().starts_with('$') {
        unreachable!()
    } else if cfg.output.is_relative() {
        opts.path.parent().unwrap().join(cfg.output)
    } else {
        cfg.output
    };

    std::fs::write(&out_path, bound)
        .with_context(|| format!("failed to write bindings to '{out_path}'"))?;

    // Now that we're done, emit the stats to let the user know exactly how
    // much was emitted
    #[derive(Default, Clone, Copy)]
    struct Stat {
        count: u32,
        total: u32,
    }

    impl<T> From<minwin::Bucket<T>> for Stat {
        #[inline]
        fn from(b: minwin::Bucket<T>) -> Self {
            Self {
                total: b.items.len() as _,
                count: b.num,
            }
        }
    }

    impl std::ops::AddAssign for Stat {
        fn add_assign(&mut self, o: Self) {
            self.count += o.count;
            self.total += o.total;
        }
    }

    use nu_ansi_term::Color;

    impl Stat {
        fn emit(self, name: &str) {
            if self.total == 0 {
                return;
            }

            let ratio = (self.count as f32 / self.total as f32) * 100.0;

            let color = if ratio < 50.0 || self.total < 50 {
                Color::Green
            } else if ratio < 75.0 {
                Color::Yellow
            } else {
                Color::Red
            };

            println!(
                "  {name}: {} / {} => {}",
                self.count,
                self.total,
                color.bold().paint(format!("{ratio:.02}%",))
            );
        }
    }

    let mut records = Stat::default();
    let mut functions = Stat::default();
    let mut constants = Stat::default();
    let mut aliases = Stat::default();
    let mut func_pointers = Stat::default();
    let mut interfaces = Stat::default();

    let num_namespaces = items.len();

    for (ns, items) in items {
        println!("{}", Color::Blue.paint(ns));

        let r = items.records.into();
        records += r;
        r.emit("records");

        let f = items.functions.into();
        functions += f;
        f.emit("functions");

        let c = items.constants.into();
        constants += c;
        c.emit("constants");

        let a = items.aliases.into();
        aliases += a;
        a.emit("aliases");

        let f = items.func_pointers.into();
        func_pointers += f;
        f.emit("func pointers");

        if !is_sys {
            let i = items.interfaces.into();
            interfaces += i;
            i.emit("interaces");
        }
    }

    let strings: &[nu_ansi_term::AnsiString<'static>] = &[
        Color::Blue.paint("Summary for "),
        Color::Blue.bold().paint(num_namespaces.to_string()),
        Color::Blue.paint(" namespaces"),
    ];

    println!("{}", nu_ansi_term::AnsiStrings(strings));
    records.emit("records");
    functions.emit("functions");
    constants.emit("constants");
    aliases.emit("aliases");
    func_pointers.emit("func pointers");

    if !is_sys {
        interfaces.emit("interfaces");
    }

    Ok(())
}
