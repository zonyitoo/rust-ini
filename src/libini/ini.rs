extern mod extra;
use std::hashmap::HashMap;
use std::hashmap::{HashMapIterator, HashMapMutIterator};
use std::path::Path;
use std::rt::io::*;
use std::cast::transmute;
use std::char;
use std::num::from_str_radix;
use std::str;

fn escape_str(s: &str) -> ~str {
    let mut escaped = ~"";
    for c in s.iter() {
        match c {
            '\\' => escaped.push_str("\\\\"),
            '\0' => escaped.push_str("\\0"),
            '\x01' .. '\x06' | '\x0E' .. '\x1F' | '\x7F' .. '\xFF'
                => escaped.push_str(format!("\\\\x{:04x}", c as int)),
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
            '\u0080' .. '\uFFFF'
                => escaped.push_str(format!("\\\\x{:04x}", c as int)),
            _ => escaped.push_char(c)
        }
    }
    escaped
}

pub struct Ini {
    priv sections: HashMap<~str, Properties>,
    priv cur_section: ~str,
    default_key: ~str,
}

type Properties = HashMap<~str, ~str>; // Key-value pairs

impl<'self> Ini {
    pub fn new() -> Ini {
        Ini {
            sections: HashMap::new(),
            cur_section: ~"@General",
            default_key: ~"@General",
        }
    }

    pub fn begin_section(&'self mut self, section: ~str) -> &'self mut Ini {
        self.cur_section = section.clone();
        self
    }

    pub fn end_section(&'self mut self) -> &'self mut Ini {
        self.cur_section = self.default_key.clone();
        self
    }

    pub fn set(&'self mut self, key: ~str, value: ~str) -> &'self mut Ini {
        {
            let dat = self.sections.find_or_insert(self.cur_section.clone(), HashMap::new());
            dat.insert_or_update_with(key, value, |_,_| {});
        }
        self
    }

    pub fn get(&'self self, key: ~str) -> &'self ~str {
        let cursec = &self.cur_section;
        let cursec_map: &'self Properties = self.sections.get(cursec);
        cursec_map.get(&key)
    }
}

impl<'self> Ini {
    pub fn write_file(&'self self, filename: &str) -> &'self Ini {
        let mut firstline = true;
        let mut file = file::open(&Path(filename), CreateOrTruncate, Write).unwrap();
        for (section, props) in self.sections.iter() {
            if firstline {
                firstline = false;
            }
            else {
                file.write("\n".as_bytes());
            }
            let section_str = format!("[{:s}]\n", escape_str(*section));
            file.write(section_str.as_bytes());
            for (k, v) in props.iter() {
                let k_str = escape_str(*k);
                let v_str = escape_str(*v);
                let prop_str = format!("{:s}={:s}\n", k_str, v_str);
                file.write(prop_str.as_bytes());
            }
        }

        self
    }
}

struct Parser<T> {
    priv ch: char,
    priv rdr: ~T,
    priv line: uint,
    priv col: uint,
}

struct Error {
    line: uint,
    col: uint,
    msg: ~str,
}

impl<T: Iterator<char>> Parser<T> {
    fn new(rdr: ~T) -> Parser<T> {
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
        self.ch == unsafe { transmute(-1u32) }
    }

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

    fn error<T>(&self, msg: ~str) -> Result<T, Error> {
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
        let mut curkey = ~"";
        let mut cursec = ~"";
        while !self.eof() {
            self.parse_whitespace();
            debug!("line:%u, col:%u", self.line, self.col);
            match self.ch {
                ';' => { 
                    self.parse_comment(); 
                    debug!("parse comment");
                }
                '[' => {
                    match self.parse_section() {
                        Ok(sec) => {
                            debug!("Got section: %s", sec);
                            cursec = sec.clone();
                            result.sections.find_or_insert(sec, HashMap::new());
                            self.bump();
                        },
                        Err(e) => return Err(e),
                    };
                }
                '=' => {
                    match self.parse_val() {
                        Ok(val) => {
                            debug!("Got value: %s", val);
                            let sec = result.sections.find_mut(&cursec).unwrap();
                            sec.insert_or_update_with(curkey, val, |_,_| {});
                            curkey = ~"";
                            self.bump();
                        },
                        Err(e) => return Err(e),
                    }
                }
                _ => {
                    match self.parse_key() {
                        Ok(key) => {
                            debug!("Got key: %s", key);
                            curkey = key.clone();
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

    fn parse_str_until(&mut self, endpoint: char) -> Result<~str, Error> {
        let mut result = ~"";
        while self.ch != endpoint {
            if self.eof() {
                return self.error(format!("Expecting \"{}\" but found EOF.", endpoint));
            }
            if self.ch == '\\' {
                self.bump();
                if self.eof() {
                    return self.error(format!("Expecting \"{}\" but found EOF.", endpoint));
                }
                match self.ch {
                    '0' => result.push_char('\0'),
                    'a' => result.push_char('\x07'),
                    'b' => result.push_char('\x08'),
                    't' => result.push_char('\t'),
                    'r' => result.push_char('\r'),
                    'n' => result.push_char('\n'),
                    '\n' => (),
                    'x' => {
                        // Unicode 4 char
                        let mut code = ~"";
                        for _ in range(0, 4) {
                            self.bump();
                            if self.eof() {
                                return self.error(format!("Expecting \"{}\" but found EOF.", endpoint));
                            }
                            else if self.ch == '\\' {
                                self.bump();
                                if self.ch != '\n' {
                                    return self.error(format!("Expecting \"\\n\" but found \"{}\".", self.ch));
                                }
                            }
                            code.push_char(self.ch);
                        }
                        let r : Option<u32> = from_str_radix(code, 16);
                        match r {
                            Some(c) => result.push_char(char::from_u32(c).unwrap()),
                            None => return self.error(~"Unknown character.")
                        }
                    }
                    _ => result.push_char(self.ch)
                }
            }
            else {
                result.push_char(self.ch);
            }
            self.bump();
        }
        Ok(result)
    }

    fn parse_section(&mut self) -> Result<~str, Error> {
        // Skip [
        self.bump();
        self.parse_str_until(']')
    }

    fn parse_key(&mut self) -> Result<~str, Error> {
        self.parse_str_until('=')
    }

    fn parse_val(&mut self) -> Result<~str, Error> {
        self.bump();
        self.parse_str_until('\n')
    }
}

impl Ini {
    pub fn load_from_str(buf : ~str) -> Ini {
        let mut parser = Parser::new(~buf.iter());
        match parser.parse() {
            Ok(ini) => ini,
            Err(e) => {
                fail!("Parse fail. %u:%u %s", e.line, e.col, e.msg);
            }
        }
    }

    pub fn load_from_file(filename : &str) -> Ini {
        let mut reader = match file::open(&Path(filename), Open, Read) {
            None => {
                fail!("File %s not exists", filename);
            }
            Some(r) => r
        };
        let mut mem = [0u8, ..10240];
        let mut buf = ~"";

        while !reader.eof() {
            let len = match reader.read(mem) {
                Some(n) => n,
                None => break
            };

            buf.push_str(str::from_utf8(mem.slice_to(len)));
        }
        Ini::load_from_str(buf) 
    }

    pub fn load_from_file_opt(filename : &str) -> Option<Ini> {
        let mut reader = match file::open(&Path(filename), Open, Read) {
            None => {
                error!("File %s not exists", filename);
                return None;
            }
            Some(r) => r
        };
        let mut mem = [0u8, ..10240];
        let mut buf = ~"";

        while !reader.eof() {
            let len = match reader.read(mem) {
                Some(n) => n,
                None => break
            };

            buf.push_str(str::from_utf8(mem.slice_to(len)));
        }
        Ini::load_from_str_opt(buf) 
    }

    pub fn load_from_str_opt(buf : ~str) -> Option<Ini> {
        let mut parser = Parser::new(~buf.iter());
        match parser.parse() {
            Ok(ini) => Some(ini),
            Err(e) => {
                error!("Parse fail. %u:%u %s", e.line, e.col, e.msg);
                None
            }
        }
    }
}

#[deriving(Clone)]
pub struct SectionIterator<'self> {
    priv mapiter: HashMapIterator<'self, ~str, Properties>
}

pub struct SectionMutIterator<'self> {
    priv mapiter: HashMapMutIterator<'self, ~str, Properties>
}

impl Ini {
    pub fn iter<'a>(&'a self) -> SectionIterator<'a> {
        SectionIterator { mapiter: self.sections.iter() }
    }

    pub fn iter_mut<'a>(&'a mut self) -> SectionMutIterator<'a> {
        SectionMutIterator { mapiter: self.sections.mut_iter() }
    }
}

impl<'self> Iterator<(&'self ~str, &'self Properties)> for SectionIterator<'self> {
    #[inline]
    fn next(&mut self) -> Option<(&'self ~str, &'self Properties)> {
        self.mapiter.next()
    }
}

impl<'self> Iterator<(&'self ~str, &'self mut Properties)> for SectionMutIterator<'self> {
    #[inline]
    fn next(&mut self) -> Option<(&'self ~str, &'self mut Properties)> {
        self.mapiter.next()
    }
}
