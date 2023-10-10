use crate::dust_json::DustJson;
use argh::FromArgs;
use colored::Colorize;
use std::fs;

#[derive(FromArgs, PartialEq)]
/// Initialize a Dust project
#[argh(subcommand, name = "init")]
pub struct Init {}

const SRC_FOLDER: &str = "src";

impl Init {
    pub async fn run(&self) {
        let current = DustJson::read();
        if current.is_some() {
            println!("dust.json already exists");
            return;
        }

        let _ = fs::create_dir(SRC_FOLDER);

        let dust_json = DustJson {
            source_directories: vec![SRC_FOLDER.to_string()],
        };

        dust_json.write().unwrap();

        println!("{}", "Done".bold().green())
    }
}
