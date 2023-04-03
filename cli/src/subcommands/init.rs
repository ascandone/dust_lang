use crate::dust_json::{DustJson, DUST_JSON_NAME};
use argh::FromArgs;
use std::fs;

#[derive(FromArgs, PartialEq)]
/// Initialize a Dust project
#[argh(subcommand, name = "init")]
pub struct Init {}

const SRC_FOLDER: &'static str = "src";

impl Init {
    pub fn run(&self) {
        // TODO check if d.json already exists
        let _ = fs::create_dir(SRC_FOLDER);

        let dust_json = DustJson {
            source_directories: vec![SRC_FOLDER.to_string()],
        };

        let serialized = serde_json::to_string_pretty(&dust_json).unwrap();

        let () = fs::write(DUST_JSON_NAME, serialized).unwrap();

        println!("Done")
    }
}
