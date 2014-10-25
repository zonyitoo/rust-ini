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

/* code */

use std::collections::hashmap::HashMap;
use std::collections::hashmap::{Entries, MutEntries};
use std::collections::hashmap::{Occupied, Vacant};
use std::io::{File, Read, Open, Write, Truncate};
use std::mem::transmute;
use std::char;
use std::num::from_str_radix;
use std::str;

#[allow(unsigned_negate)]
fn eof() -> char { unsafe {transmute(-1u32)} }

fn escape_str(s: &str) -> String {
    let mut escaped: String = "".to_string();
    for c in s.chars() {
        match c {
            '\\' => escaped.push_str("\\\\"),
            '\0' => escaped.push_str("\\0"),
            '\x01' ... '\x06' | '\x0E' ... '\x1F' | '\x7F' ... '\xFF' =>
                escaped.push_str(format!("\\x{:04x}", c as int).as_slice()),
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
            '\u0080' ... '\uFFFF' =>
                escaped.push_str(format!("\\x{:04x}", c as int).as_slice()),
            _ => escaped.push(c)
        }
    }
    escaped
}

pub struct Ini {
    sections: HashMap<String, Properties>,
    cur_section: String,
    pub default_key: String,
}

pub type Properties = HashMap<String, String>; // Key-value pairs

static GENERAL_SECTION_KEY: &'static str = "@General";

impl<'a> Ini {
    pub fn new() -> Ini {
        Ini {
            sections: HashMap::new(),
            cur_section: GENERAL_SECTION_KEY.to_string(),
            default_key: GENERAL_SECTION_KEY.to_string(),
        }
    }

    pub fn begin_section(&'a mut self, section: &str) -> &'a mut Ini {
        self.cur_section = section.to_string();
        self
    }

    pub fn end_section(&'a mut self) -> &'a mut Ini {
        self.cur_section = self.default_key.clone();
        self
    }

    pub fn set(&'a mut self, key: &str, value: &str) -> &'a mut Ini {
        {
            let dat = match self.sections.entry(self.cur_section.clone()) {
                Vacant(entry) => entry.set(HashMap::new()),
                Occupied(entry) => entry.into_mut(),
            };
            match dat.entry(key.to_string()) {
                Vacant(entry) => entry.set(value.to_string()),
                Occupied(mut entry) => {
                    *entry.get_mut() = value.to_string();
                    entry.into_mut()
                },
            };
        }
        self
    }

    pub fn get(&'a self, key: &str) -> &'a String {
        &self.sections[self.cur_section][key.to_string()]
    }
}

impl<'a> Ini {
    #[allow(unused_must_use)]
    pub fn write_file(&'a self, filename: &str) -> &'a Ini {
        let mut firstline = true;
        let mut file = File::open_mode(&Path::new(filename), Truncate, Write).unwrap();
        for (section, props) in self.sections.iter() {
            if firstline {
                firstline = false;
            }
            else {
                file.write("\n".as_bytes());
            }
            let section_str = format!("[{:s}]\n", escape_str(section.as_slice()));
            file.write(section_str.as_slice().as_bytes());
            for (k, v) in props.iter() {
                let k_str = escape_str(k.as_slice());
                let v_str = escape_str(v.as_slice());
                let prop_str = format!("{:s}={:s}\n", k_str, v_str);
                file.write(prop_str.as_bytes());
            }
        }

        self
    }
}

struct Parser<T> {
    ch: char,
    rdr: Box<T>,
    line: uint,
    col: uint,
}

struct Error {
    line: uint,
    col: uint,
    msg: String,
}

impl<T: Iterator<char>> Parser<T> {
    fn new(rdr: Box<T>) -> Parser<T> {
        let mut p = Parser {
            ch: '\x00',
            line: 0,
            col: 0,
            rdr: rdr
        };
        p.bump();
        p
    }

    fn eof(&self) -> bool {
        self.ch == eof()
    }

    #[allow(unsigned_negate)]
    fn bump(&mut self) {
        match self.rdr.next() {
            Some(ch) => self.ch = ch,
            None => self.ch = unsafe { transmute(-1u32) }
        }

        if self.ch == '\n' {
            self.line += 1u;
            self.col = 0u;
        }
        else {
            self.col += 1u;
        }
    }

    fn error<T>(&self, msg: String) -> Result<T, Error> {
        Err(Error { line: self.line, col: self.col, msg: msg.clone() })
    }

    fn parse_whitespace(&mut self) {
        while self.ch == ' ' ||
            self.ch == '\n' ||
            self.ch == '\t' ||
            self.ch == '\r' { self.bump(); }
    }

    pub fn parse(&mut self) -> Result<Ini, Error> {
        self.parse_whitespace();
        let mut result = Ini::new();
        let mut curkey: String = "".to_string();
        let mut cursec: String = "".to_string();
        while !self.eof() {
            self.parse_whitespace();
            debug!("line:{}, col:{}", self.line, self.col);
            match self.ch {
                ';' => {
                    self.parse_comment();
                    debug!("parse comment");
                }
                '[' => {
                    match self.parse_section() {
                        Ok(sec) => {
                            let msec = sec.as_slice().trim();
                            debug!("Got section: {}", msec);
                            cursec = msec.to_string();
                            match result.sections.entry(cursec.clone()) {
                                Vacant(entry) => entry.set(HashMap::new()),
                                Occupied(entry) => entry.into_mut(),
                            };
                            self.bump();
                        },
                        Err(e) => return Err(e),
                    };
                }
                '=' => {
                    if curkey.as_slice().char_len() == 0 {
                        return self.error("Missing key".to_string());
                    }
                    match self.parse_val() {
                        Ok(val) => {
                            let mval = val.as_slice().trim();
                            debug!("Got value: {}", mval);
                            let sec = result.sections.find_mut(&cursec).unwrap();
                            match sec.entry(curkey) {
                                Vacant(entry) => entry.set(mval.to_string()),
                                Occupied(mut entry) => {
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
                            let mkey = key.as_slice().trim();
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
        while self.ch != '\n' && !self.eof() { self.bump(); }
        if !self.eof() { self.bump(); }
    }

    fn parse_str_until(&mut self, endpoint: &[char]) -> Result<String, Error> {
        let mut result: String = "".to_string();
        while !endpoint.contains(&self.ch) {
            if self.eof() {
                return self.error(format!("Expecting \"{}\" but found EOF.", endpoint));
            }
            if self.ch == '\\' {
                self.bump();
                if self.eof() {
                    return self.error(format!("Expecting \"{}\" but found EOF.", endpoint));
                }
                match self.ch {
                    '0' => result.push('\0'),
                    'a' => result.push('\x07'),
                    'b' => result.push('\x08'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    'n' => result.push('\n'),
                    '\n' => (),
                    'x' => {
                        // Unicode 4 char
                        let mut code: String = "".to_string();
                        for _ in range(0u, 4u) {
                            self.bump();
                            if self.eof() {
                                return self.error(format!("Expecting \"{}\" but found EOF.", endpoint));
                            }
                            else if self.ch == '\\' {
                                self.bump();
                                if self.ch != '\n' {
                                    return self.error(format!("Expecting \"\\\\n\" but found \"{}\".", self.ch));
                                }
                            }
                            code.push(self.ch);
                        }
                        let r : Option<u32> = from_str_radix(code.as_slice(), 16);
                        match r {
                            Some(c) => result.push(char::from_u32(c).unwrap()),
                            None => return self.error("Unknown character.".to_string())
                        }
                    }
                    _ => result.push(self.ch)
                }
            }
            else {
                result.push(self.ch);
            }
            self.bump();
        }
        Ok(result)
    }

    fn parse_section(&mut self) -> Result<String, Error> {
        // Skip [
        self.bump();
        self.parse_str_until([']'])
    }

    fn parse_key(&mut self) -> Result<String, Error> {
        self.parse_str_until(['='])
    }

    fn parse_val(&mut self) -> Result<String, Error> {
        self.bump();
        self.parse_str_until(['\n', eof()])
    }
}

impl Ini {
    pub fn load_from_str(buf: String) -> Ini {
        let mut parser = Parser::new(box buf.as_slice().chars());
        match parser.parse() {
            Ok(ini) => ini,
            Err(e) => {
                fail!("Parse fail. {}:{} {}", e.line, e.col, e.msg);
            }
        }
    }

    pub fn load_from_file(filename : &str) -> Ini {
        let mut reader = match File::open_mode(&Path::new(filename), Open, Read) {
            Err(..) => {
                fail!("File {} not exists", filename);
            }
            Ok(r) => r
        };
        let mut mem = [0u8, ..10240];
        let mut buf: String = "".to_string();

        while !reader.eof() {
            let len = match reader.read(mem) {
                Ok(n) => n,
                Err(..) => break
            };

            buf.push_str(str::from_utf8(mem.slice_to(len)).unwrap());
        }
        Ini::load_from_str(buf)
    }

    pub fn load_from_file_opt(filename : &str) -> Option<Ini> {
        let mut reader = match File::open_mode(&Path::new(filename), Open, Read) {
            Err(..) => {
                error!("File {} not exists", filename);
                return None;
            }
            Ok(r) => r
        };
        let mut mem = [0u8, ..10240];
        let mut buf: String = "".to_string();

        while !reader.eof() {
            let len = match reader.read(mem) {
                Ok(n) => n,
                Err(..) => break
            };

            buf.push_str(str::from_utf8(mem.slice_to(len)).unwrap());
        }
        Ini::load_from_str_opt(buf)
    }

    pub fn load_from_str_opt(buf : String) -> Option<Ini> {
        let mut parser = Parser::new(box buf.as_slice().chars());
        match parser.parse() {
            Ok(ini) => Some(ini),
            Err(e) => {
                error!("Parse fail. {}:{} {}", e.line, e.col, e.msg);
                None
            }
        }
    }
}

pub struct SectionIterator<'a> {
    mapiter: Entries<'a, String, Properties>
}

pub struct SectionMutIterator<'a> {
    mapiter: MutEntries<'a, String, Properties>
}

impl Ini {
    pub fn iter<'a>(&'a self) -> SectionIterator<'a> {
        SectionIterator { mapiter: self.sections.iter() }
    }

    pub fn mut_iter<'a>(&'a mut self) -> SectionMutIterator<'a> {
        SectionMutIterator { mapiter: self.sections.iter_mut() }
    }
}

impl<'a> Iterator<(&'a String, &'a Properties)> for SectionIterator<'a> {
    #[inline]
    fn next(&mut self) -> Option<(&'a String, &'a Properties)> {
        self.mapiter.next()
    }
}

impl<'a> Iterator<(&'a String, &'a mut Properties)> for SectionMutIterator<'a> {
    #[inline]
    fn next(&mut self) -> Option<(&'a String, &'a mut Properties)> {
        self.mapiter.next()
    }
}

//------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use ini::*;

    #[test]
    fn load_from_str_opt_with_valid_input() {
        let input = "[sec1]\nkey1=val1\nkey2=377\n[sec2]foo=bar\n".to_string();
        let opt = Ini::load_from_str_opt(input);
        assert!(opt.is_some());

        let output = opt.unwrap();
        assert_eq!(output.sections.len(), 2);
        assert!(output.sections.contains_key(&"sec1".to_string()));

        let sec1 = &output.sections["sec1".to_string()];
        assert_eq!(sec1.len(), 2);
        assert!(sec1.contains_key(&"key1".to_string()));
        assert!(sec1.contains_key(&"key2".to_string()));
        assert_eq!(sec1["key1".to_string()], "val1".to_string());
        assert_eq!(sec1["key2".to_string()], "377".to_string());

    }

    #[test]
    fn load_from_str_opt_without_ending_newline() {
        let input = "[sec1]\nkey1=val1\nkey2=377\n[sec2]foo=bar".to_string();
        let opt = Ini::load_from_str_opt(input);
        assert!(opt.is_some());
    }
}
