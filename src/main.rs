use anyhow::Context as _;
use camino::Utf8PathBuf as PathBuf;
use clap::Parser;

#[derive(clap::Parser)]
struct Cmd {
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

fn main() -> anyhow::Result<()> {
    use tracing_subscriber::prelude::*;

    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let opts = Cmd::parse();

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
