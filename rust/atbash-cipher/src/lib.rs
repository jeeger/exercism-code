fn atbash_map_char(c: char) -> Option<char> {
    if c.is_ascii_alphabetic() {
        Some(('z' as u8 - c as u8 + 'a' as u8) as char)
    } else if c.is_numeric() {
        Some(c)
    } else {
        None
    }
}


fn atbash_unmap_char(c: char) -> Option<char> {
    if c.is_ascii_alphabetic() {
        Some(('a' as u8 + ('z' as u8 - c as u8)) as char)
    } else if c.is_numeric() {
      Some(c)  
    } else {
        None
    }
}

/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
    plain.chars()
        .filter_map(|c| atbash_map_char(c.to_ascii_lowercase()))
        .enumerate()
        .map(|(idx, val)| if idx % 5 == 0 && idx != 0 {
            format!(" {}", val)
        } else {
            val.to_string()
        })
        .collect()
}

/// "Decipher" with the Atbash cipher.
pub fn decode(cipher: &str) -> String {
    cipher
        .chars()
        .filter_map(|c| atbash_unmap_char(c))
        .collect()
}
