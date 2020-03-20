pub fn reply(message: &str) -> &str {
    fn question(m: &str) -> bool {
        m.ends_with('?')
    }
    fn yelled(m: &str) -> bool {
        m.chars().filter(|c| c.is_alphabetic()).count() > 0 &&
            m.chars().filter(|c| c.is_alphabetic()).all(|c| c.is_uppercase())
    }
    fn nothing(m: &str) -> bool {
        m.is_empty()
    }
    let clean = message.trim();
    if nothing(clean) {
        // Is there a way to skip the return here?
        return "Fine. Be that way!";
    }
    match (yelled(clean), question(clean)) {
        (true, true) => "Calm down, I know what I'm doing!",
        (false, true) => "Sure.",
        (true, false) => "Whoa, chill out!",
        (false, false) => "Whatever."
    }
}
