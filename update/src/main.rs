fn main() {
    // 1. Read the VCS info for windows-metadata from ~/.cargo/registry/src/github.com-1ecc6299db9ec823/windows-metadata-<version>/.cargo_vcs_info.json
    // 2. Clone https://github.com/microsoft/windows-rs and checkout the commit from the previous step
    // 3. Copy the .winmd files from crates/libs/metadata/default to this repo
}
