fn main() {
    let md = cargo_metadata::MetadataCommand::new()
        .manifest_path("Cargo.toml")
        .exec()
        .expect("failed to retrieve metadata");

    let wm = md
        .packages
        .iter()
        .find(|pkg| pkg.name == "windows-metadata")
        .expect("failed to find windows-metadata");

    // Read the VCS info for windows-metadata from ~/.cargo/registry/src/github.com-1ecc6299db9ec823/windows-metadata-<version>/.cargo_vcs_info.json
    let vcs_info_path = wm
        .manifest_path
        .parent()
        .unwrap()
        .join(".cargo_vcs_info.json");

    #[derive(serde::Deserialize)]
    struct Git {
        sha1: String,
    }

    #[derive(serde::Deserialize)]
    struct VcsInfo {
        git: Git,
        path_in_vcs: String,
    }

    let vcs_info =
        std::fs::read_to_string(vcs_info_path).expect("failed to read .cargo_vcs_info.json");
    let vcs_info: VcsInfo =
        serde_json::from_str(&vcs_info).expect("failed to deserialize .cargo_vcs_info.json");

    let winmd_files = [
        "Windows.Win32.Interop.winmd",
        "Windows.Win32.winmd",
        "Windows.winmd",
    ];

    let url_root = format!(
        "https://github.com/microsoft/windows-rs/raw/{}/{}/default/",
        vcs_info.git.sha1, vcs_info.path_in_vcs
    );

    let md_dir = std::path::Path::new("md");

    if md_dir.exists() {
        std::fs::remove_dir_all(&md_dir).expect("failed to remove 'md'");
    }

    std::fs::create_dir(&md_dir).expect("failed to create 'md'");

    let client = reqwest::blocking::Client::new();

    use rayon::prelude::*;
    winmd_files.into_par_iter().for_each(|winmd| {
        println!("Downloading {winmd}");
        let buffer = client
            .get(format!("{url_root}{winmd}"))
            .send()
            .expect("failed to send request")
            .error_for_status()
            .expect("failed to retrieve winmd file")
            .bytes()
            .expect("failed to read response");

        println!("Compressing {winmd}");
        let compressed =
            zstd::encode_all(std::io::Cursor::new(buffer), 5).expect("failed to compress");

        println!("Writing {winmd}");
        std::fs::write(md_dir.join(winmd).with_extension("winmd.zstd"), compressed)
            .expect("failed to write winmd file");
    });
}
