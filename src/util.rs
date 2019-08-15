use std::fs::File;
use std::io::prelude::*;

pub fn bool_not(b: &bool) -> bool {
    !b
}

pub fn bool_is(b: &bool) -> bool {
    *b
}

const U32_ZERO: u32 = 0;

pub fn u32_is_zero(v: &u32) -> bool {
    *v == U32_ZERO
}

#[allow(dead_code)]
pub fn retain_json_string(s: &mut String) {
    s.retain(|c| !c.is_whitespace());
}

#[allow(dead_code)]
pub fn read_file_into_string(file_path: &String, contents: &mut String) -> std::io::Result<()> {
    let mut file = File::open(file_path)?;
    file.read_to_string(contents)?;
    Ok(())
}
