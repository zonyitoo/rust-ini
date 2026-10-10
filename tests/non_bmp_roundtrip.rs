use ini::{EscapePolicy, Ini, LineSeparator, ParseOption, WriteOption};

fn roundtrip_extended(policy: EscapePolicy) {
    let section = "sec";
    let key = "key";
    let value = "\u{10000}0 \u{1f431}face \u{fffff}abc \u{100000}F \u{10ffff}42 \u{ffff}a \\x1234";
    let mut ini = Ini::new();
    ini.with_section(Some(section)).set(key, value);
    let mut output = Vec::new();
    ini.write_to_opt(
        &mut output,
        WriteOption {
            escape_policy: policy,
            line_separator: LineSeparator::CR,
            kv_separator: "=",
            ..WriteOption::default()
        },
    )
    .unwrap();
    let output = String::from_utf8(output).unwrap();
    let reloaded = Ini::load_from_str(&output).unwrap();
    assert_eq!(reloaded.get_from(Some(section), key), Some(value));
    assert!(output.contains("\\U0001f431"), "{:?}", output);
    assert!(output.contains("\\U0010ffff"), "{:?}", output);
}

#[test]
fn non_bmp_roundtrip_basics_extended() {
    roundtrip_extended(EscapePolicy::BasicsUnicodeExtended);
}

#[test]
fn non_bmp_roundtrip_reserved_extended() {
    roundtrip_extended(EscapePolicy::ReservedUnicodeExtended);
}

#[test]
fn non_bmp_roundtrip_everything() {
    roundtrip_extended(EscapePolicy::Everything);
}

#[test]
fn non_bmp_parse_long_escape_with_following_hex_digits() {
    let ini = Ini::load_from_str("[s]\nk=\\U0001F431FACE\\x0061f\n").unwrap();
    assert_eq!(ini.get_from(Some("s"), "k"), Some("\u{1f431}FACEaf"));
}

#[test]
fn non_bmp_reject_invalid_long_escapes() {
    for escape in ["\\U0001f43", "\\U0001f43G", "\\U0000d800", "\\U00110000", "\\U+0000041"] {
        assert!(Ini::load_from_str(&format!("k={escape}\n")).is_err(), "{:?}", escape);
    }
}

#[test]
fn non_bmp_disabled_escaping_remains_literal() {
    let ini = Ini::load_from_str_opt(
        "k=\\U0001f431\n",
        ParseOption {
            enabled_escape: false,
            ..ParseOption::default()
        },
    )
    .unwrap();
    assert_eq!(ini.get_from(None::<&str>, "k"), Some("\\U0001f431"));
}

#[test]
fn non_bmp_bmp_escapes_keep_four_digit_boundary() {
    let ini = Ini::load_from_str("k=\\x1f431\n").unwrap();
    assert_eq!(ini.get_from(None::<&str>, "k"), Some("\u{1f43}1"));
}

#[test]
fn non_bmp_names_roundtrip() {
    let mut ini = Ini::new();
    let section = "sec\u{1f431}face";
    let key = "key\u{10ffff}42";
    ini.with_section(Some(section)).set(key, "value");
    let mut output = Vec::new();
    ini.write_to_opt(
        &mut output,
        WriteOption {
            escape_policy: EscapePolicy::ReservedUnicodeExtended,
            ..WriteOption::default()
        },
    )
    .unwrap();
    let reloaded = Ini::load_from_str(&String::from_utf8(output).unwrap()).unwrap();
    assert_eq!(reloaded.get_from(Some(section), key), Some("value"));
}
