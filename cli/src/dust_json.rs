use serde::{Deserialize, Serialize};

pub const DUST_JSON_NAME: &'static str = "dust.json";

#[derive(Serialize, Deserialize)]
pub struct DustJson {
    #[serde(rename = "source-directories")]
    pub source_directories: Vec<String>,
}
