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

use std::collections::HashMap;
use std::collections::hash_map::{Iter, IterMut, Keys};
use std::collections::hash_map::Entry;
use std::fs::{OpenOptions, File};
use std::ops::{Index, IndexMut};
use std::char;
use std::io::{self, Write, Read, BufReader, Cursor};
use std::fmt::{self, Display};
use std::path::Path;

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

pub struct SectionSetter<'a> {
    ini: &'a mut Ini,
    section_name: Option<String>,
}

impl<'a> SectionSetter<'a> {
    fn new(ini: &'a mut Ini, section_name: Option<String>) -> SectionSetter<'a> {
        SectionSetter {
            ini: ini,
            section_name: section_name,
        }
    }

    pub fn set(&'a mut self, key: &str, value: &str) -> &'a mut SectionSetter<'a> {
        {
            let prop = match self.ini.sections.entry(self.section_name.clone()) {
                Entry::Vacant(entry) => entry.insert(HashMap::new()),
                Entry::Occupied(entry) => entry.into_mut(),
            };
            match prop.entry(key.to_owned()) {
                Entry::Vacant(entry) => entry.insert(value.to_owned()),
                Entry::Occupied(mut entry) => {
                    *entry.get_mut() = value.to_owned();
                    entry.into_mut()
                },
            };
        }
        self
    }

    pub fn get(&'a mut self, key: &str) -> Option<&'a str> {
        let prop = match self.ini.sections.entry(self.section_name.clone()) {
            Entry::Vacant(entry) => entry.insert(HashMap::new()),
            Entry::Occupied(entry) => entry.into_mut(),
        };
        prop.get(&key.to_owned()).map(|s| &s[..])
    }
}

pub type Properties = HashMap<String, String>; // Key-value pairs

pub struct Ini {
    sections: HashMap<Option<String>, Properties>,
}

impl<'a> Ini {
    pub fn new() -> Ini {
        Ini {
            sections: {
                let mut props = HashMap::new();
                props.insert(None, Properties::new());
                props
            },
        }
    }

    pub fn begin_section(&'a mut self, section: Option<&str>) -> SectionSetter<'a> {
        SectionSetter::new(self, section.map(|s| s.to_owned()))
    }

    pub fn general_section(&'a self) -> &'a Properties {
        let idx: Option<&str> = None;
        self.index(&idx)
    }

    pub fn general_section_mut(&'a mut self) -> &'a mut Properties {
        let idx: Option<&str> = None;
        self.index_mut(&idx)
    }

    pub fn section(&'a self, name: Option<&str>) -> Option<&'a Properties> {
        self.sections.get(&name.map(|s| s.to_owned()))
    }

    pub fn section_mut(&'a mut self, name: Option<&str>) -> Option<&'a mut Properties> {
        self.sections.get_mut(&name.map(|s| s.to_owned()))
    }

    pub fn entry(&mut self, name: Option<String>) -> Entry<Option<String>, Properties> {
        self.sections.entry(name)
    }

    pub fn clear(&'a mut self) {
        self.sections.clear()
    }

    pub fn sections(&'a self) -> Keys<'a, Option<String>, Properties> {
        self.sections.keys()
    }

    pub fn get_from(&'a self, section: Option<&str>, key: &str) -> Option<&'a str> {
        match self.sections.get(&section.map(|s| s.to_owned())) {
            None => None,
            Some(ref prop) => {
                match prop.get(&key.to_owned()) {
                    Some(p) => Some(&p[..]),
                    None => None
                }
            }
        }
    }

    pub fn get_from_or(&'a self, section: Option<&str>, key: &str, default: &'a str) -> &'a str {
         match self.sections.get(&section.map(|s| s.to_owned())) {
            None => default,
            Some(ref prop) => {
                match prop.get(&key.to_owned()) {
                    Some(p) => &p[..],
                    None => default
                }
            }
        }
    }

    pub fn get_from_mut(&'a mut self, section: Option<&str>, key: &str) -> Option<&'a mut String> {
        match self.sections.get_mut(&section.map(|s| s.to_owned())) {
            None => None,
            Some(mut prop) => {
                prop.get_mut(&key.to_owned())
            }
        }
    }
}

impl<'q> Index<&'q Option<&'q str>> for Ini {
    type Output = Properties;

    fn index<'a>(&'a self, index: &Option<&'q str>) -> &'a Properties {
        match self.sections.get(&index.map(|s| s.to_owned())) {
            Some(p) => p,
            None => panic!("Section `{:?}` does not exists", index),
        }
    }
}

impl<'q> IndexMut<&'q Option<&'q str>> for Ini {
    fn index_mut<'a>(&'a mut self, index: &Option<&'q str>) -> &'a mut Properties {
        match self.sections.get_mut(&index.map(|s| s.to_owned())) {
            Some(p) => p,
            None => panic!("Section `{:?}` does not exists", index)
        }
    }
}

impl<'q> Index<&'q str> for Ini {
    type Output = Properties;

    fn index<'a>(&'a self, index: &'q str) -> &'a Properties {
        match self.sections.get(&Some(index.to_owned())) {
            Some(p) => p,
            None => panic!("Section `{}` does not exists", index),
        }
    }
}

impl<'q> IndexMut<&'q str> for Ini {
    fn index_mut<'a>(&'a mut self, index: &'q str) -> &'a mut Properties {
        match self.sections.get_mut(&Some(index.to_owned())) {
            Some(p) => p,
            None => panic!("Section `{}` does not exists", index)
        }
    }
}

impl<'q> Index<&'q Option<String>> for Ini {
    type Output = Properties;

    fn index<'a>(&'a self, index: &'q Option<String>) -> &'a Properties {
        match self.sections.get(index) {
            Some(p) => p,
            None => panic!("Section `{:?}` does not exists", index),
        }
    }
}

impl<'q> IndexMut<&'q Option<String>> for Ini {
    fn index_mut<'a>(&'a mut self, index: &'q Option<String>) -> &'a mut Properties {
        match self.sections.get_mut(index) {
            Some(p) => p,
            None => panic!("Section `{:?}` does not exists", index)
        }
    }
}

impl<'q> Index<&'q String> for Ini {
    type Output = Properties;

    fn index<'a>(&'a self, index: &'q String) -> &'a Properties {
        match self.sections.get(&Some(index.clone())) {
            Some(p) => p,
            None => panic!("Section `{}` does not exists", index),
        }
    }
}

impl<'q> IndexMut<&'q String> for Ini {
    fn index_mut<'a>(&'a mut self, index: &'q String) -> &'a mut Properties {
        match self.sections.get_mut(&Some(index.clone())) {
            Some(p) => p,
            None => panic!("Section `{}` does not exists", index)
        }
    }
}

impl Ini {
    pub fn write_to_file(&self, filename: &str) -> io::Result<()> {
        let mut file = try!(OpenOptions::new().write(true).truncate(true).create(true).open(&Path::new(filename)));
        self.write_to(&mut file)
    }

    pub fn write_to(&self, writer: &mut Write) -> io::Result<()> {
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

impl Ini {
    pub fn load_from_str(buf: &str) -> Result<Ini, Error> {
        let bufreader = BufReader::new(Cursor::new(buf.as_bytes().to_vec()));
        let mut parser = Parser::new(bufreader.chars());
        parser.parse()
    }

    pub fn read_from(reader: &mut Read) -> Result<Ini, Error> {
        let bufr = BufReader::new(reader);
        let mut parser = Parser::new(bufr.chars());
        parser.parse()
    }

    pub fn load_from_file(filename : &str) -> Result<Ini, Error> {
        let mut reader = match File::open(&Path::new(filename)) {
            Err(e) => {
                return Err(Error {line: 0, col: 0, msg: format!("Unable to open `{}`: {}", filename, e)})
            }
            Ok(r) => r
        };
        Ini::read_from(&mut reader)
    }
}

pub struct SectionIterator<'a> {
    mapiter: Iter<'a, Option<String>, Properties>
}

pub struct SectionMutIterator<'a> {
    mapiter: IterMut<'a, Option<String>, Properties>
}

impl Ini {
    pub fn iter<'a>(&'a self) -> SectionIterator<'a> {
        SectionIterator { mapiter: self.sections.iter() }
    }

    pub fn mut_iter<'a>(&'a mut self) -> SectionMutIterator<'a> {
        SectionMutIterator { mapiter: self.sections.iter_mut() }
    }
}

impl<'a> Iterator for SectionIterator<'a> {
    type Item = (&'a Option<String>, &'a Properties);

    #[inline]
    fn next(&mut self) -> Option<(&'a Option<String>, &'a Properties)> {
        self.mapiter.next()
    }
}

impl<'a> Iterator<> for SectionMutIterator<'a> {
    type Item = (&'a Option<String>, &'a mut Properties);

    #[inline]
    fn next(&mut self) -> Option<(&'a Option<String>, &'a mut Properties)> {
        self.mapiter.next()
    }
}

struct Parser<R: Read> {
    ch: Option<char>,
    rdr: io::Chars<R>,
    line: usize,
    col: usize,
}

#[derive(Debug)]
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

impl<R: Read> Parser<R> {
    pub fn new(rdr: io::Chars<R>) -> Parser<R> {
        let mut p = Parser {
            ch: None,
            line: 0,
            col: 0,
            rdr: rdr
        };
        p.bump();
        p
    }

    fn eof(&self) -> bool {
        self.ch.is_none()
    }

    #[allow(unsigned_negation)]
    fn bump(&mut self) {
        match self.rdr.next() {
            Some(Ok(ch)) => self.ch = Some(ch),
            _ => self.ch = None,
        }
        match self.ch {
            Some(ch) => {
                if ch == '\n' {
                    self.line += 1;
                    self.col = 0;
                }
                else {
                    self.col += 1;
                }
            },
            None => {},
        }
    }

    fn error<U>(&self, msg: String) -> Result<U, Error> {
        Err(Error { line: self.line, col: self.col, msg: msg.clone() })
    }

    fn parse_whitespace(&mut self) {
        while self.ch.unwrap() == ' ' ||
            self.ch.unwrap() == '\n' ||
            self.ch.unwrap() == '\t' ||
            self.ch.unwrap() == '\r' { self.bump(); }
    }

    pub fn parse(&mut self) -> Result<Ini, Error> {
        self.parse_whitespace();
        let mut result = Ini::new();
        let mut curkey: String = "".to_owned();
        let mut cursec: Option<String> = None;
        while !self.eof() {
            self.parse_whitespace();
            debug!("line:{}, col:{}", self.line, self.col);
            match self.ch.unwrap() {
                ';' => {
                    self.parse_comment();
                    debug!("parse comment");
                }
                '[' => {
                    match self.parse_section() {
                        Ok(sec) => {
                            let msec = &sec[..].trim();
                            debug!("Got section: {}", msec);
                            cursec = Some(msec.to_string());
                            match result.sections.entry(cursec.clone()) {
                                Entry::Vacant(entry) => entry.insert(HashMap::new()),
                                Entry::Occupied(entry) => entry.into_mut(),
                            };
                            self.bump();
                        },
                        Err(e) => return Err(e),
                    };
                }
                '=' => {
                    if (&curkey[..]).chars().count() == 0 {
                        return self.error("Missing key".to_string());
                    }
                    match self.parse_val() {
                        Ok(val) => {
                            let mval = &val[..].trim();
                            debug!("Got value: {}", mval);
                            let sec = result.sections.get_mut(&cursec).unwrap();
                            match sec.entry(curkey) {
                                Entry::Vacant(entry) => entry.insert(mval.to_string()),
                                Entry::Occupied(mut entry) => {
                                    *entry.get_mut() = mval.to_string();
                                    entry.into_mut()
                                },
                            };
                            curkey = "".to_string();
                            self.bump();
                        },
                        Err(e) => return Err(e),
                    }
                }
                _ => {
                    match self.parse_key() {
                        Ok(key) => {
                            let mkey = &key[..].trim();
                            debug!("Got key: {}", mkey);
                            curkey = mkey.to_string();
                        }
                        Err(e) => return Err(e),
                    }
                }
            }
        }

        Ok(result)
    }

    fn parse_comment(&mut self)  {
        while self.ch.unwrap() != '\n' && !self.eof() { self.bump(); }
        if !self.eof() { self.bump(); }
    }

    fn parse_str_until(&mut self, endpoint: &[Option<char>]) -> Result<String, Error> {
        let mut result: String = "".to_string();
        while !endpoint.contains(&self.ch) {
            if self.eof() {
                return self.error(format!("Expecting \"{:?}\" but found EOF.", endpoint));
            }
            if self.ch.unwrap() == '\\' {
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
                        let mut code: String = "".to_string();
                        for _ in 0..4 {
                            self.bump();
                            if self.eof() {
                                return self.error(format!("Expecting \"{:?}\" but found EOF.", endpoint));
                            }
                            else if self.ch.unwrap() == '\\' {
                                self.bump();
                                if self.ch.unwrap() != '\n' {
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
                    _ => result.push(self.ch.unwrap())
                }
            }
            else {
                result.push(self.ch.unwrap());
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
    use ini::*;

    #[test]
    fn load_from_str_with_valid_input() {
        let input = "[sec1]\nkey1=val1\nkey2=377\n[sec2]foo=bar\n";
        let opt = Ini::load_from_str(input);
        assert!(opt.is_ok());

        let output = opt.unwrap();
        assert_eq!(output.sections.len(), 2);
        assert!(output.sections.contains_key(&Some("sec1".to_owned())));

        let sec1 = &output.sections[&Some("sec1".to_owned())];
        assert_eq!(sec1.len(), 2);
        assert!(sec1.contains_key(&"key1".to_owned()));
        assert!(sec1.contains_key(&"key2".to_owned()));
        assert_eq!(sec1[&"key1".to_owned()], "val1".to_owned());
        assert_eq!(sec1[&"key2".to_owned()], "377".to_owned());

    }

    #[test]
    fn load_from_str_without_ending_newline() {
        let input = "[sec1]\nkey1=val1\nkey2=377\n[sec2]foo=bar";
        let opt = Ini::load_from_str(input);
        assert!(opt.is_ok());
    }
}
