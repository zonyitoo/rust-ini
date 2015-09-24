// The MIT License (MIT)

// Copyright (c) 2014 Y. T. CHUNG

// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

//! Ini

use std::collections::HashMap;
use std::collections::hash_map::{Iter, IterMut, Keys};
use std::collections::hash_map::Entry;
use std::fs::{OpenOptions, File};
use std::ops::{Index, IndexMut};
use std::char;
use std::io::{self, Write, Read};
use std::fmt::{self, Display};
use std::path::Path;
use std::borrow::Cow;
use std::str::Chars;

// Escape non-INI characters
//
// Common escape sequences: https://en.wikipedia.org/wiki/INI_file#Escape_characters
//
// * `\\` \ (a single backslash, escaping the escape character)
// * `\0` Null character
// * `\a` Bell/Alert/Audible
// * `\b` Backspace, Bell character for some applications
// * `\t` Tab character
// * `\r` Carriage return
// * `\n` Line feed
// * `\;` Semicolon
// * `\#` Number sign
// * `\=` Equals sign
// * `\:` Colon
// * `\x????` Unicode character with hexadecimal code point corresponding to ????
fn escape_str(s: &str) -> String {
    let mut escaped: String = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => escaped.push_str("\\\\"),
            '\0' => escaped.push_str("\\0"),
            '\x01' ... '\x06' | '\x0e' ... '\x1f' | '\x7f' ... '\u{00ff}' =>
                escaped.push_str(&format!("\\x{:04x}", c as isize)[..]),
            '\x07' => escaped.push_str("\\a"),
            '\x08' => escaped.push_str("\\b"),
            '\x0c' => escaped.push_str("\\f"),
            '\x0b' => escaped.push_str("\\v"),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            '\r' => escaped.push_str("\\r"),
            ';' => escaped.push_str("\\;"),
            '#' => escaped.push_str("\\#"),
            '=' => escaped.push_str("\\="),
            ':' => escaped.push_str("\\:"),
            '\u{0080}' ... '\u{FFFF}' =>
                escaped.push_str(&format!("\\x{:04x}", c as isize)[..]),

            // FIXME: Ini files does not support unicode code point in \u{100000} to \u{10FFFF}
            _ => escaped.push(c)
        }
    }
    escaped
}

/// A setter which could be used to set key-value pair in a specified section
pub struct SectionSetter<'a, 'i: 'a, 'pk: 'i, 'pv: 'i> {
    ini: &'a mut Ini<'i, 'pk, 'pv>,
    section_name: Option<Cow<'i, str>>,
}

impl<'a, 'i: 'a, 'pk: 'i, 'pv: 'i> SectionSetter<'a, 'i, 'pk, 'pv> {
    fn new(ini: &'a mut Ini<'i, 'pk, 'pv>, section_name: Option<Cow<'i, str>>) -> SectionSetter<'a, 'i, 'pk, 'pv> {
        SectionSetter {
            ini: ini,
            section_name: section_name,
        }
    }

    /// Set key-value pair in this section
    pub fn set(&'a mut self, key: &'pk str, value: &'pv str) -> &'a mut SectionSetter<'a, 'i, 'pk, 'pv> {
        {
            let prop = match self.ini.sections.entry(self.section_name.clone()) {
                Entry::Vacant(entry) => entry.insert(HashMap::new()),
                Entry::Occupied(entry) => entry.into_mut(),
            };
            prop.insert(key.into(), value.into());
        }
        self
    }

    /// Delete the entry in this section with `key`
    pub fn delete(&'a mut self, key: &str) -> &'a mut SectionSetter<'a, 'i, 'pk, 'pv> {
        if let Some(prop) = self.ini.sections.get_mut(&self.section_name) {
            prop.remove(key);
        }
        self
    }

    /// Get the entry in this section with `key`
    pub fn get(&'a mut self, key: &str) -> Option<&'a str> {
        let prop = match self.ini.sections.entry(self.section_name.clone()) {
            Entry::Vacant(entry) => entry.insert(HashMap::new()),
            Entry::Occupied(entry) => entry.into_mut(),
        };
        prop.get(key).map(|s| &s[..])
    }
}

/// Properties type (key-value pairs)
pub type Properties<'k, 'v> = HashMap<Cow<'k, str>, Cow<'v, str>>; // Key-value pairs

/// Ini struct
pub struct Ini<'a, 'pk: 'a, 'pv: 'a> {
    sections: HashMap<Option<Cow<'a, str>>, Properties<'pk, 'pv>>,
}

impl<'i, 'pk: 'i, 'pv: 'i> Ini<'i, 'pk, 'pv> {
    /// Create an instance
    pub fn new() -> Ini<'i, 'pk, 'pv> {
        Ini {
            sections: HashMap::new(),
        }
    }

    /// Set with a specified section, `None` is for the general section
    pub fn with_section<'b, 's: 'i>(&'b mut self, section: Option<&'s str>) -> SectionSetter<'b, 'i, 'pk, 'pv> {
        SectionSetter::new(self, section.map(|s| s.into()))
    }

    /// Get the immmutable general section
    pub fn general_section<'a: 'i>(&'a self) -> &'i Properties<'pk, 'pv> {
        self.section(None).expect("There is no general section in this Ini")
    }

    /// Get the mutable general section
    pub fn general_section_mut<'a: 'i>(&'a mut self) -> &'i mut Properties<'pk, 'pv> {
        self.section_mut(None).expect("There is no general section in this Ini")
    }

    /// Get a immutable section
    pub fn section<'a: 'i, 'p>(&'a self, name: Option<&'p str>) -> Option<&'i Properties<'pk, 'pv>> {
        self.sections.get(&name.map(|s| s.into()))
    }

    /// Get a mutable section
    pub fn section_mut<'a, 'p: 'i>(&'a mut self, name: Option<&'p str>) -> Option<&'a mut Properties<'pk, 'pv>> {
        self.sections.get_mut(&name.map(|s| s.into()))
    }

    /// Get the entry
    pub fn entry<'a>(&'a mut self, name: Option<String>) -> Entry<Option<Cow<'i, str>>, Properties<'pk, 'pv>> {
        self.sections.entry(name.map(|s| s.into()))
    }

    /// Clear all entries
    pub fn clear<'a>(&mut self) {
        self.sections.clear()
    }

    /// Iterate with sections
    pub fn sections<'a: 'i>(&'a self) -> Keys<'a, Option<Cow<'i, str>>, Properties<'pk, 'pv>> {
        self.sections.keys()
    }

    /// Set key-value to a section
    pub fn set_to(&mut self, section: Option<&'i str>, key: &'pk str, value: &'pv str) {
        self.with_section(section).set(key, value);
    }

    /// Get the value from a section with key
    pub fn get_from<'a>(&'a self, section: Option<&'a str>, key: &str) -> Option<&'a str> {
        match self.sections.get(&section.map(|s| s.into())) {
            None => None,
            Some(ref prop) => {
                match prop.get(key) {
                    Some(p) => Some(&p[..]),
                    None => None
                }
            }
        }
    }

    /// Get the value from a section with key, return the default value if it does not exists
    pub fn get_from_or<'a>(&'a self, section: Option<&'a str>, key: &str, default: &'a str) -> &'a str {
         match self.sections.get(&section.map(|s| s.into())) {
            None => default,
            Some(ref prop) => {
                match prop.get(key) {
                    Some(p) => &p[..],
                    None => default
                }
            }
        }
    }

    /// Get the mutable from a section with key
    pub fn get_from_mut<'a: 'i>(&'a mut self, section: Option<&'a str>, key: &'pk str)
            -> Option<&'i mut Cow<'pv, str>> {
        match self.sections.get_mut(&section.map(|s| s.into())) {
            None => None,
            Some(mut prop) => {
                let key: Cow<'pk, str> = key.into();
                prop.get_mut(&key)
            }
        }
    }

    /// Delete a section, return the properties if it exists
    pub fn delete(&mut self, section: Option<&'i str>) -> Option<Properties<'pk, 'pv>> {
        self.sections.remove(&section.map(|s| s.into()))
    }

    pub fn delete_from(&mut self, section: Option<&'i str>, key: &str) -> Option<Cow<'pv, str>> {
        match self.section_mut(section) {
            None => return None,
            Some(prop) => prop.remove(key),
        }
    }
}

impl<'i, 'pk: 'i, 'pv: 'i, 'q> Index<&'q Option<&'q str>> for Ini<'i, 'pk, 'pv> {
    type Output = Properties<'pk, 'pv>;

    fn index<'a>(&'a self, index: &Option<&'q str>) -> &'a Properties<'pk, 'pv> {
        match self.sections.get(&index.map(|s| s.into())) {
            Some(p) => p,
            None => panic!("Section `{:?}` does not exists", index),
        }
    }
}

impl<'i, 'pk: 'i, 'pv: 'i> IndexMut<&'i Option<&'i str>> for Ini<'i, 'pk, 'pv> {
    fn index_mut<'a>(&'a mut self, index: &Option<&'i str>) -> &'a mut Properties<'pk, 'pv> {
        match self.sections.get_mut(&index.map(|s| s.into())) {
            Some(p) => p,
            None => panic!("Section `{:?}` does not exists", index)
        }
    }
}

impl<'q, 'pk: 'q, 'pv: 'q> Index<&'q str> for Ini<'q, 'pk, 'pv> {
    type Output = Properties<'pk, 'pv>;

    fn index<'a>(&'a self, index: &'q str) -> &'a Properties<'pk, 'pv> {
        match self.sections.get(&Some(index.into())) {
            Some(p) => p,
            None => panic!("Section `{}` does not exists", index),
        }
    }
}

impl<'q, 'pk: 'q, 'pv: 'q> IndexMut<&'q str> for Ini<'q, 'pk, 'pv> {
    fn index_mut<'a>(&'a mut self, index: &'q str) -> &'a mut Properties<'pk, 'pv> {
        match self.sections.get_mut(&Some(index.into())) {
            Some(p) => p,
            None => panic!("Section `{}` does not exists", index)
        }
    }
}

impl<'i, 'pk: 'i, 'pv: 'i> Ini<'i, 'pk, 'pv> {
    /// Write to a file
    pub fn write_to_file(&'i self, filename: &str) -> io::Result<()> {
        let mut file = try!(OpenOptions::new().write(true).truncate(true).create(true).open(&Path::new(filename)));
        self.write_to(&mut file)
    }

    /// Write to a writer
    pub fn write_to<W: Write>(&'i self, writer: &mut W) -> io::Result<()> {
        let mut firstline = true;

        match self.sections.get(&None) {
            Some(props) => {
                for (k, v) in props.iter() {
                    let k_str = escape_str(&k[..]);
                    let v_str = escape_str(&v[..]);
                    try!(write!(writer, "{}={}\n", k_str, v_str));
                }
                firstline = false;
            },
            None => {}
        }

        for (section, props) in self.sections.iter().filter(|&(ref s, _)| s.is_some()) {
            if firstline {
                firstline = false;
            }
            else {
                try!(writer.write_all(b"\n"));
            }

            if let &Some(ref section) = section {
                try!(write!(writer, "[{}]\n", escape_str(&section[..])));

                for (k, v) in props.iter() {
                    let k_str = escape_str(&k[..]);
                    let v_str = escape_str(&v[..]);
                    try!(write!(writer, "{}={}\n", k_str, v_str));
                }
            }
        }
        Ok(())
    }
}

impl<'i, 'pk: 'i, 'pv: 'i> Ini<'i, 'pk, 'pv> {
    /// Load from a string
    pub fn load_from_str(buf: &str) -> Result<Ini<'i, 'pk, 'pv>, Error> {
        let mut parser = Parser::new(buf.chars());
        parser.parse()
    }

    /// Load from a reader
    pub fn read_from<R: Read>(reader: &mut R) -> Result<Ini<'i, 'pk, 'pv>, Error> {
        let mut s = String::new();
        try!(reader.read_to_string(&mut s).map_err(|err| Error {
            line: 0,
            col: 0,
            msg: format!("{}", err),
        }));
        let mut parser = Parser::new(s.chars());
        parser.parse()
    }

    /// Load from a file
    pub fn load_from_file(filename : &str) -> Result<Ini<'i, 'pk, 'pv>, Error> {
        let mut reader = match File::open(&Path::new(filename)) {
            Err(e) => {
                return Err(Error {line: 0, col: 0, msg: format!("Unable to open `{}`: {}", filename, e)})
            }
            Ok(r) => r
        };
        Ini::read_from(&mut reader)
    }
}

/// Iterator for sections
pub struct SectionIterator<'a, 'pk: 'a, 'pv: 'a> {
    mapiter: Iter<'a, Option<Cow<'a, str>>, Properties<'pk, 'pv>>
}

/// Iterator for mutable sections
pub struct SectionMutIterator<'a, 'pk: 'a, 'pv: 'a> {
    mapiter: IterMut<'a, Option<Cow<'a, str>>, Properties<'pk, 'pv>>
}

impl<'a, 'pk: 'a, 'pv: 'a> Ini<'a, 'pk, 'pv> {
    /// Immutable iterate though sections
    pub fn iter(&'a self) -> SectionIterator<'a, 'pk, 'pv> {
        SectionIterator { mapiter: self.sections.iter() }
    }

    /// Mutable iterate though sections
    pub fn mut_iter(&'a mut self) -> SectionMutIterator<'a, 'pk, 'pv> {
        SectionMutIterator { mapiter: self.sections.iter_mut() }
    }
}

impl<'a, 'pk: 'a, 'pv: 'a> Iterator for SectionIterator<'a, 'pk, 'pv> {
    type Item = (&'a Option<Cow<'a, str>>, &'a Properties<'pk, 'pv>);

    #[inline]
    fn next(&mut self) -> Option<(&'a Option<Cow<'a, str>>, &'a Properties<'pk, 'pv>)> {
        self.mapiter.next()
    }
}

impl<'a, 'pk: 'a, 'pv: 'a> Iterator<> for SectionMutIterator<'a, 'pk, 'pv> {
    type Item = (&'a Option<Cow<'a, str>>, &'a mut Properties<'pk, 'pv>);

    #[inline]
    fn next(&mut self) -> Option<(&'a Option<Cow<'a, str>>, &'a mut Properties<'pk, 'pv>)> {
        self.mapiter.next()
    }
}

// Ini parser
struct Parser<'a> {
    ch: Option<char>,
    rdr: Chars<'a>,
    line: usize,
    col: usize,
}

#[derive(Debug)]
/// Parse error
pub struct Error {
    pub line: usize,
    pub col: usize,
    pub msg: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} {}", self.line, self.col, self.msg)
    }
}

impl<'a> Parser<'a> {
    // Create a parser
    pub fn new(rdr: Chars<'a>) -> Parser<'a> {
        let mut p = Parser {
            ch: None,
            line: 0,
            col: 0,
            rdr: rdr,
        };
        p.bump();
        p
    }

    fn eof(&self) -> bool {
        self.ch.is_none()
    }

    fn bump(&mut self) {
        self.ch = self.rdr.next();
        match self.ch {
            Some('\n') => {
                self.line += 1;
                self.col = 0;
            },
            Some(..) => {
                self.col += 1;
            }
            None => {},
        }
    }

    fn error<U>(&self, msg: String) -> Result<U, Error> {
        Err(Error { line: self.line, col: self.col, msg: msg.clone() })
    }

    fn parse_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if !c.is_whitespace() && c != '\n' && c != '\t' && c != '\r' {
                break;
            }
            self.bump();
        }
    }

    pub fn parse<'i, 'pk: 'i, 'pv: 'i>(&mut self) -> Result<Ini<'i, 'pk, 'pv>, Error> {
        self.parse_whitespace();
        let mut result = Ini::new();
        let mut curkey: Cow<'pk, str> = "".into();
        let mut cursec: Option<Cow<'i, str>> = None;
        while let Some(cur_c) = self.ch {
            self.parse_whitespace();
            debug!("line:{}, col:{}", self.line, self.col);
            match cur_c {
                ';' => {
                    self.parse_comment();
                    debug!("parse comment");
                }
                '[' => {
                    match self.parse_section() {
                        Ok(sec) => {
                            let msec = &sec[..].trim();
                            debug!("Got section: {}", msec);
                            cursec = Some(Cow::Owned(msec.to_string()));
                            result.sections.entry(cursec.clone()).or_insert(HashMap::new());
                            self.bump();
                        },
                        Err(e) => return Err(e),
                    };
                }
                '=' => {
                    if (&curkey[..]).is_empty() {
                        return self.error("Missing key".to_string());
                    }
                    match self.parse_val() {
                        Ok(val) => {
                            let mval = val[..].trim().to_owned();
                            debug!("Got value: {}", mval);
                            let sec = result.sections.entry(cursec.clone()).or_insert(HashMap::new());
                            sec.insert(curkey, Cow::Owned(mval));
                            curkey = "".into();
                            self.bump();
                        },
                        Err(e) => return Err(e),
                    }
                }
                _ => {
                    match self.parse_key() {
                        Ok(key) => {
                            let mkey: String = key[..].trim().to_owned();
                            debug!("Got key: {}", mkey);
                            curkey = mkey.into();
                        }
                        Err(e) => return Err(e),
                    }
                }
            }
        }

        Ok(result)
    }

    fn parse_comment(&mut self)  {
        while let Some(c) = self.ch { self.bump(); if c == '\n' { break; }  }
    }

    fn parse_str_until(&mut self, endpoint: &[Option<char>]) -> Result<String, Error> {
        let mut result: String = String::new();
        while !endpoint.contains(&self.ch) {
            match self.ch {
                None => {
                    return self.error(format!("Expecting \"{:?}\" but found EOF.", endpoint));
                },
                Some('\\') => {
                    self.bump();
                    if self.eof() {
                        return self.error(format!("Expecting \"{:?}\" but found EOF.", endpoint));
                    }
                    match self.ch.unwrap() {
                        '0' => result.push('\0'),
                        'a' => result.push('\x07'),
                        'b' => result.push('\x08'),
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        'n' => result.push('\n'),
                        '\n' => (),
                        'x' => {
                            // Unicode 4 character
                            let mut code: String = String::with_capacity(4);
                            for _ in 0..4 {
                                self.bump();
                                if self.eof() {
                                    return self.error(format!("Expecting \"{:?}\" but found EOF.", endpoint));
                                }
                                else if let Some('\\') = self.ch {
                                    self.bump();
                                    if self.ch != Some('\n') {
                                        return self.error(format!("Expecting \"\\\\n\" but found \"{:?}\".", self.ch));
                                    }
                                }
                                code.push(self.ch.unwrap());
                            }
                            let r = u32::from_str_radix(&code[..], 16);
                            match r {
                                Ok(c) => result.push(char::from_u32(c).unwrap()),
                                Err(_) => return self.error("Unknown character.".to_string())
                            }
                        }
                        c => result.push(c)
                    }
                },
                Some(c) => {
                    result.push(c);
                }
            }
            self.bump();
        }
        Ok(result)
    }

    fn parse_section(&mut self) -> Result<String, Error> {
        // Skip [
        self.bump();
        self.parse_str_until(&[Some(']')])
    }

    fn parse_key(&mut self) -> Result<String, Error> {
        self.parse_str_until(&[Some('=')])
    }

    fn parse_val(&mut self) -> Result<String, Error> {
        self.bump();
        self.parse_str_until(&[Some('\n'), None])
    }
}

//------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use ini::*;

    #[test]
    fn load_from_str_with_valid_input() {
        let input = "[sec1]\nkey1=val1\nkey2=377\n[sec2]foo=bar\n";
        let opt = Ini::load_from_str(input);
        assert!(opt.is_ok());

        let output = opt.unwrap();
        assert_eq!(output.sections.len(), 2);
        assert!(output.sections.contains_key(&Some("sec1".into())));

        let sec1 = &output.sections[&Some("sec1".into())];
        assert_eq!(sec1.len(), 2);
        let key1: Cow<'static, str> = "key1".into();
        assert!(sec1.contains_key(&key1));
        let key2: Cow<'static, str> = "key2".into();
        assert!(sec1.contains_key(&key2));
        let val1: Cow<'static, str> = "val1".into();
        assert_eq!(sec1[&key1], val1);
        let val2: Cow<'static, str> = "377".into();
        assert_eq!(sec1[&key2], val2);

    }

    #[test]
    fn load_from_str_without_ending_newline() {
        let input = "[sec1]\nkey1=val1\nkey2=377\n[sec2]foo=bar";
        let opt = Ini::load_from_str(input);
        assert!(opt.is_ok());
    }

    #[test]
    fn test_parse_comment() {
        let input = "; abcdefghijklmn\n";
        let opt = Ini::load_from_str(input);
        assert!(opt.is_ok());
    }
}
