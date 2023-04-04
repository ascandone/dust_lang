use serde::{Deserialize, Serialize};
use std::fs;

const DUST_JSON_NAME: &'static str = "dust.json";

#[derive(Serialize, Deserialize)]
pub struct DustJson {
    #[serde(rename = "source-directories")]
    pub source_directories: Vec<String>,
}

impl DustJson {
    pub fn read() -> Option<DustJson> {
        let content = fs::read_to_string(DUST_JSON_NAME).ok()?;
        let dust_json = serde_json::from_str::<DustJson>(&content).ok()?;
        Some(dust_json)
    }

    pub fn write(&self) -> Result<(), ()> {
        let serialized = serde_json::to_string_pretty(self).unwrap();
        let () = fs::write(DUST_JSON_NAME, serialized).map_err(|_| ())?;
        Ok(())
    }
}
