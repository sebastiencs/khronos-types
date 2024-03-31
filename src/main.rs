use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::Cursor;
use std::iter::Peekable;
use std::slice::Iter;

use quick_xml::events::Event;
use quick_xml::name::QName;
use quick_xml::reader::Reader;

fn get_attributes(event: &Event) -> HashMap<String, String> {
    let attrs = match &event {
        Event::Start(e) => e.attributes(),
        Event::Empty(e) => e.attributes(),
        _ => return HashMap::default(),
    };
    let attrs = attrs
        .map(|a| {
            let attr = a.unwrap();
            let key = std::str::from_utf8(&attr.key.0).unwrap();
            let value = std::str::from_utf8(&attr.value).unwrap();
            (key.to_string(), value.to_string())
        })
        .collect();
    attrs
}

fn get_name<'a>(event: &'a Event) -> Option<&'a str> {
    let name = match &event {
        Event::Start(e) => e.name(),
        Event::End(e) => e.name(),
        Event::Empty(e) => e.name(),
        _ => return None,
    };
    Some(std::str::from_utf8(&name.0).unwrap())
}

struct Parser {
    buffer: Vec<u8>,
    reader: Reader<Cursor<String>>,
}

impl Parser {
    fn new(s: String) -> Self {
        let mut reader = Reader::from_reader(Cursor::new(s.to_string()));
        reader.trim_text(true);
        Self {
            reader,
            buffer: Vec::with_capacity(32 * 1024),
        }
    }

    fn next(&mut self) -> Option<Event<'_>> {
        self.buffer.clear();
        match self.reader.read_event_into(&mut self.buffer) {
            Ok(Event::Eof) => None,
            Ok(e) => Some(e),
            Err(_e) => todo!(),
        }
    }
}

fn is_end_of(name: &str, event: &Event) -> bool {
    match &event {
        Event::End(end) if end.name() == QName(name.as_bytes()) => true,
        _ => false,
    }
}

fn collect_until_end_of(until: &str, parser: &mut Parser) -> Vec<Event<'static>> {
    let mut events = Vec::with_capacity(32);
    while let Some(event) = parser.next() {
        if is_end_of(until, &event) {
            break;
        }
        let e = event.into_owned();
        events.push(e)
    }
    events
}

#[derive(Debug, PartialEq, Eq, Ord)]
struct EnumValue {
    key: String,
    value: String,
    comment: Option<String>,
    alias: Option<String>,
}

impl PartialOrd for EnumValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.key.partial_cmp(&other.key)
    }
}

#[derive(Debug)]
struct Enums {
    name: String,
    comment: Option<String>,
    values: Vec<EnumValue>,
}

fn parse_enum_value(e: &Event) -> Option<EnumValue> {
    let attrs = get_attributes(e);
    if !(attrs.contains_key("value") && attrs.contains_key("name")) {
        return None;
    }
    let key = {
        let key = attrs.get("name").unwrap();
        key.strip_prefix("EGL_").unwrap_or(key).to_string()
    };
    let value = attrs.get("value").unwrap().clone();
    let comment = attrs.get("comment").cloned();
    let alias = {
        attrs
            .get("alias")
            .map(|alias| alias.strip_prefix("EGL_").unwrap_or(alias).to_string())
    };
    Some(EnumValue {
        key,
        value,
        comment,
        alias,
    })
}

fn parse_enums(attrs: HashMap<String, String>, parser: &mut Parser) -> Enums {
    let enums = collect_until_end_of("enums", parser);

    let name = attrs.get("namespace").unwrap().to_string();
    let comment = attrs.get("comment").map(|s| s.to_string());
    let values = enums.iter().filter_map(parse_enum_value).collect();

    Enums {
        name,
        comment,
        values,
    }
}

fn parse_constants(parser: &mut Parser) -> Vec<EnumValue> {
    let enums = collect_until_end_of("enums", parser);
    enums.iter().filter_map(parse_enum_value).collect()
}

#[derive(Debug)]
struct Method {
    return_type: String,
    method_name: String,
    params: Vec<(String, String)>,
}

fn parse_command_line(
    expected: &str,
    commands: &mut Peekable<Iter<'_, Event<'_>>>,
) -> (String, String) {
    assert_eq!(get_name(&commands.next().unwrap()), Some(expected));

    let mut return_value = Vec::<u8>::with_capacity(16);
    while let Some(e) = commands.peek() {
        if get_name(e) == Some("name") {
            break;
        }
        let Event::Text(return_type) = commands.next().unwrap().clone() else {
            continue;
        };
        if !return_value.is_empty() {
            return_value.push(b' ');
        }
        return_value.extend(return_type.iter());
    }
    let return_value = String::from_utf8(return_value).unwrap();

    assert_eq!(get_name(&commands.next().unwrap()), Some("name"));
    let Event::Text(method_name) = commands.next().unwrap().clone() else {
        panic!();
    };
    let method_name = String::from_utf8(method_name.to_vec()).unwrap();
    assert_eq!(get_name(&commands.next().unwrap()), Some("name")); // end
    assert_eq!(get_name(&commands.next().unwrap()), Some(expected)); // end

    (return_value, method_name)
}

fn parse_commands(parser: &mut Parser, commands_map: &mut BTreeMap<String, Method>) {
    let commands = collect_until_end_of("command", parser);
    let mut commands = commands.iter().peekable();

    let (return_type, method_name) = parse_command_line("proto", &mut commands);

    let mut params = Vec::with_capacity(8);
    while let Some(next) = commands.peek() {
        if !(get_name(*next) == Some("param")) {
            break;
        }
        let (param_type, param_name) = parse_command_line("param", &mut commands);
        params.push((param_type, param_name));
    }

    const IGNORE: [Option<&str>; 3] = [Some("alias"), Some("glx"), Some("vecequiv")];

    while let Some(next) = commands.peek() {
        if !IGNORE.contains(&get_name(*next)) {
            break;
        }
        let _ = commands.next();
    }

    assert!(commands.next().is_none());

    commands_map.insert(
        method_name.clone(),
        Method {
            return_type,
            method_name,
            params,
        },
    );
}

fn parse_extensions(parser: &mut Parser, extension_commands: &mut BTreeSet<String>) {
    let exts = collect_until_end_of("extensions", parser);
    let mut exts = exts.iter();

    while let Some(next) = exts.next() {
        match get_name(next) {
            None | Some("enum" | "extension" | "require" | "type") => {}
            Some("command") => {
                extension_commands.insert(get_attributes(next).get("name").unwrap().clone());
            }
            e => todo!("{:?}", e),
        }
    }
}

fn write_rust_declarations<'a, O: std::io::Write>(
    enums: &[Enums],
    constants: &BTreeSet<EnumValue>,
    commands: &BTreeMap<String, Method>,
    extension_commands: &BTreeMap<&String, Method>,
    output: &mut O,
) {
    let write_comment = |prefix: &str, s: &Option<String>, output: &mut O| {
        if let Some(s) = s {
            writeln!(output, "{} {}", prefix, s).unwrap();
        };
    };

    writeln!(output, "// {} constants:", constants.len()).unwrap();
    for EnumValue {
        key,
        value,
        comment,
        alias,
    } in constants.iter()
    {
        write_comment("///", comment, output);
        write_comment("/// alias:", alias, output);
        writeln!(output, "pub const {}: core::ffi::c_uint = {}", key, value).unwrap();
    }

    writeln!(output, "\n// {} enums:", enums.len()).unwrap();
    for Enums {
        name,
        comment,
        values,
    } in enums
    {
        if let Some(comment) = comment {
            writeln!(output, "/// {}", comment).unwrap();
        };
        writeln!(output, "#[repr(u32)]\npub enum {} {{", name).unwrap();
        for EnumValue {
            key,
            value,
            comment,
            alias,
        } in values
        {
            write_comment("  ///", comment, output);
            write_comment("  /// alias:", alias, output);
            writeln!(output, "  {} = {},", key, value).unwrap();
        }
        writeln!(output, "}}").unwrap();
    }

    writeln!(output, "\n// {} methods:", commands.len()).unwrap();
    for Method {
        return_type,
        method_name,
        params,
    } in commands.values()
    {
        write!(output, "extern \"C\" unsafe fn {}(", method_name).unwrap();
        let params = params
            .iter()
            .map(|(type_name, name)| format!("{}: {}", name, type_name))
            .collect::<Vec<_>>();
        write!(output, "{}", params.join(", ")).unwrap();
        writeln!(output, ") -> {};", return_type).unwrap();
    }

    writeln!(
        output,
        "\n// {} extension methods:",
        extension_commands.len()
    )
    .unwrap();
    for Method {
        return_type,
        method_name,
        params,
    } in extension_commands.values()
    {
        write!(
            output,
            "fn load_{}(loader: impl Fn(&str) -> Option<*mut core::ffi::c_void>)",
            method_name
        )
        .unwrap();
        write!(output, "-> Option<unsafe extern \"C\" fn (").unwrap();
        let params = params
            .iter()
            .map(|(type_name, _name)| type_name.as_str())
            .collect::<Vec<_>>();
        write!(output, "{}", params.join(", ")).unwrap();
        writeln!(output, ") -> {}> {{", return_type).unwrap();
        writeln!(
            output,
            "  loader(\"{}\").map(|ptr| unsafe {{ std::mem::transmute(ptr) }})",
            method_name
        )
        .unwrap();
        writeln!(output, "}}").unwrap()
    }
}

fn parse_xml(path: &str, exclude_namespace: &str) {
    let s = std::fs::read_to_string(path).unwrap();

    let mut parser = Parser::new(s);

    // skip first lines
    while let Some(event) = parser.next() {
        if is_end_of("types", &event) {
            break;
        }
    }

    let mut enums = Vec::with_capacity(1024);
    let mut constants = BTreeSet::new();
    let mut commands = BTreeMap::new();
    let mut extension_commands = BTreeSet::new();

    while let Some(event) = parser.next() {
        match &event {
            Event::Comment(_) => continue,
            Event::Start(_) => match &get_name(&event) {
                Some("enums") => {
                    let attrs = get_attributes(&event);
                    if attrs.get("namespace").unwrap() != exclude_namespace {
                        enums.push(parse_enums(attrs, &mut parser));
                    } else {
                        constants.extend(parse_constants(&mut parser));
                    }
                }
                Some("command") => {
                    parse_commands(&mut parser, &mut commands);
                }
                Some("extensions") => {
                    parse_extensions(&mut parser, &mut extension_commands);
                }
                Some("feature") => {
                    let _ = collect_until_end_of("feature", &mut parser);
                }
                e => eprintln!("ignore {:?}", e),
            },
            e => {
                eprintln!("ignoring: {:?}", e);
            }
        }
    }

    // Extract extensions
    let extension_commands = extension_commands
        .iter()
        .map(|ext| (ext, commands.remove(ext).unwrap()))
        .collect::<BTreeMap<_, _>>();

    write_rust_declarations(
        &enums,
        &constants,
        &commands,
        &extension_commands,
        &mut std::io::stdout(),
    );
}

fn main() {
    parse_xml("gl.xml", "GL");
    parse_xml("egl.xml", "EGL");
}
