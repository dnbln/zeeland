use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::bail;
use clap::Parser;

#[derive(Debug, Clone, Hash)]
struct ZeelandApiLocation {
    crate_path: PathBuf,
    trait_path_in_crate: String,
}

fn zeeland_api_location_hash(api: &ZeelandApiLocation) -> CargoZeelandResult<u64> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    let canonical_location = std::fs::canonicalize(&api.crate_path)?;
    canonical_location.hash(&mut hasher);
    api.trait_path_in_crate.hash(&mut hasher);
    Ok(hasher.finish())
}

impl FromStr for ZeelandApiLocation {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (crate_path, trait_path_in_crate) =
            s.split_once(':').ok_or(anyhow::anyhow!("Invalid format"))?;

        Ok(Self {
            crate_path: crate_path.into(),
            trait_path_in_crate: trait_path_in_crate.into(),
        })
    }
}

#[derive(Parser, Debug)]
enum App {
    InitServerCrate {
        #[clap(long)]
        path: Option<PathBuf>,
        api: ZeelandApiLocation,
    },
    RunTempServer {
        api: ZeelandApiLocation,
    },
}

#[derive(Parser, Debug)]
#[command(name = "cargo")]
#[command(bin_name = "cargo")]
enum CargoApp {
    Zeeland {
        #[clap(subcommand)]
        app: App,
    },
}

type CargoZeelandResult<T = ()> = anyhow::Result<T>;

fn command_output(
    name: impl AsRef<OsStr>,
    cmd: impl FnOnce(&mut std::process::Command) -> &mut std::process::Command,
) -> CargoZeelandResult<String> {
    let mut command = std::process::Command::new(name);
    let command = cmd(&mut command);

    let output = command.output().unwrap();
    if !output.status.success() {
        bail!("Command failed: {:?}: {}", command, output.status);
    }

    Ok(String::from_utf8(output.stdout).unwrap())
}

fn run_command(
    name: impl AsRef<OsStr>,
    cmd: impl FnOnce(&mut std::process::Command) -> &mut std::process::Command,
) -> CargoZeelandResult {
    let mut command = std::process::Command::new(name);
    let command = cmd(&mut command);

    let status = command.status().unwrap();
    if !status.success() {
        bail!("Command failed: {:?}: {}", command, status);
    }

    Ok(())
}

fn init_server_crate(path: &PathBuf, api: &ZeelandApiLocation) -> CargoZeelandResult {
    println!("Initializing server crate at {}", path.display());

    let cargo_toml = path.join("Cargo.toml");
    let api_crate = get_dep_name(&api.crate_path)?;
    let zeeland_spec = std::env::var("ZEELAND_SPEC")
        .unwrap_or_else(|_| format!(r#""{}""#, env!("CARGO_PKG_VERSION")));

    if !cargo_toml.exists() || std::env::var("ZEELAND_RELOAD_CARGO_TOML").is_ok_and(|it| it == "1")
    {
        std::fs::create_dir_all(&path)?;
        std::fs::write(
            &cargo_toml,
            format!(
                r#"
[package]
name = "server"
version = "0.1.0"
edition = "2021"

[dependencies]
rocket = {{version = "0.5", features = ["json"]}}
serde = {{version = "1", features = ["derive"]}}
{api_crate} = {{ path = "{api_crate_path}" }}
zeeland = {zeeland_spec}

[workspace]
"#,
                api_crate_path = pathdiff::diff_paths(&api.crate_path.canonicalize()?, &path)
                    .unwrap()
                    .display(),
            ),
        )?;
    }

    let api_trait_name = api.trait_path_in_crate.split("::").last().unwrap();

    let rocket_macro_name = format!("{api_trait_name}_rocket",);
    let make_server_impls_macro_name = format!("{api_trait_name}_make_server_impls",);

    let trait_path_in_crate = &api.trait_path_in_crate;
    let dep_name = get_dep_name(&api.crate_path)?;

    std::fs::create_dir_all(path.join("src"))?;

    std::fs::write(
        path.join("src/main.rs"),
        format!(
            r#"
#![allow(unused_imports)]
use {dep_name}::{{{make_server_impls_macro_name}, {rocket_macro_name}, {trait_path_in_crate}}};
use zeeland::thiserror;

{make_server_impls_macro_name}!();
{rocket_macro_name}!();
"#
        )
        .trim_start(),
    )?;

    Ok(())
}

fn get_target_dir() -> CargoZeelandResult<PathBuf> {
    let output = command_output("cargo", |cargo| cargo.arg("metadata"))?;
    let metadata: serde_json::Value = serde_json::from_str(&output)?;

    let target_dir = metadata["target_directory"].as_str().unwrap();
    Ok(target_dir.into())
}

fn main() -> CargoZeelandResult {
    let CargoApp::Zeeland { app } = CargoApp::parse();

    match app {
        App::InitServerCrate { path, api } => {
            let path = path.unwrap_or_else(|| api.crate_path.join("server"));
            init_server_crate(&path, &api)?;
        }
        App::RunTempServer { api } => {
            let target_dir = get_target_dir()?;
            let path = target_dir.join(format!(
                "zeeland_temp_server/{}",
                zeeland_api_location_hash(&api)?
            ));
            init_server_crate(&path, &api)?;

            run_command("cargo", |cargo| {
                cargo
                    .arg("run")
                    .arg("--manifest-path")
                    .arg(path.join("Cargo.toml"))
            })?;
        }
    }

    Ok(())
}

fn get_dep_name(crate_path: impl AsRef<Path>) -> CargoZeelandResult<String> {
    let cargo_toml = crate_path.as_ref().join("Cargo.toml");
    let output = command_output("cargo", |cargo| {
        cargo
            .arg("read-manifest")
            .arg("--manifest-path")
            .arg(&cargo_toml)
    })?;

    #[derive(serde::Deserialize)]
    struct PkgInfo {
        name: String,
    }

    let pkg_info: PkgInfo = serde_json::from_str(&output)?;

    Ok(pkg_info.name)
}
