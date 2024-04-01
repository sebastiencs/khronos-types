use std::borrow::Cow;
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
    let key = attrs.get("name").unwrap().clone();
    let value = attrs.get("value").unwrap().clone();
    let comment = attrs.get("comment").cloned();
    let alias = attrs.get("alias").cloned();
    Some(EnumValue {
        key,
        value,
        comment,
        alias,
    })
}

fn parse_enums(attrs: HashMap<String, String>, parser: &mut Parser) -> Enums {
    let enums = collect_until_end_of("enums", parser);

    let name = match attrs.get("group") {
        Some(name) => name.to_string(),
        None => attrs.get("namespace").unwrap().to_string(),
    };
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

fn strip_prefix<'a>(prefix: &str, s: &'a str) -> &'a str {
    s.strip_prefix(prefix).unwrap_or(s)
}

fn with_c_void(s: &str) -> &str {
    match s {
        "void" => "core::ffi::c_void",
        "int" => "core::ffi::c_int",
        "char" => "core::ffi::c_char",
        "struct AHardwareBuffer" => "AHardwareBuffer",
        s => s,
    }
}

// TODO: Maybe use `regex` here ?
fn convert_param_type(type_name: &str) -> Cow<'_, str> {
    if let Some(type_name) = type_name.strip_suffix(" *const*") {
        let type_name = type_name.strip_prefix("const ").unwrap();
        Cow::Owned(format!("*const *const {}", with_c_void(type_name)))
    } else if let Some(type_name) = type_name.strip_suffix(" *") {
        if let Some(type_name) = type_name.strip_prefix("const ") {
            Cow::Owned(format!("*const {}", with_c_void(type_name)))
        } else {
            let type_name = type_name.strip_prefix("struct ").unwrap_or(type_name);
            Cow::Owned(format!("*mut {}", with_c_void(type_name)))
        }
    } else if let Some(type_name) = type_name.strip_suffix(" **") {
        if let Some(type_name) = type_name.strip_prefix("const ") {
            Cow::Owned(format!("*const *const {}", with_c_void(type_name)))
        } else {
            Cow::Owned(format!("*mut *mut {}", with_c_void(type_name)))
        }
    } else {
        Cow::Borrowed(type_name)
    }
}

fn rename_param(param_name: &str) -> &str {
    match param_name {
        "type" => "r#type",
        "ref" => "r#ref",
        s => s,
    }
}

fn make_constant_type(value: &str) -> &str {
    if value.starts_with("EGL_CAST(") {
        assert!(value.ends_with(")"));
        let sep = value.find(',').unwrap();
        let type_name = &value[9..sep];
        type_name
    } else if value == "0xFFFFFFFFFFFFFFFF" {
        "core::ffi::c_ulonglong"
    } else {
        "core::ffi::c_uint"
    }
}

fn make_constant_value(s: &str) -> Cow<'_, str> {
    if s.starts_with("-") {
        Cow::Owned(format!("{} as _", s))
    } else if s.starts_with("EGL_CAST(") {
        assert!(s.ends_with(")"));
        let sep = s.find(',').unwrap();
        let type_name = &s[9..sep];
        let value = &s[sep + 1..s.len() - 1];
        Cow::Owned(format!("{} as {}", value, type_name))
    } else {
        Cow::Borrowed(s)
    }
}

fn write_rust_declarations<'a, O: std::io::Write>(
    params: ParseParams<O>,
    enums: &[Enums],
    constants: &BTreeSet<EnumValue>,
    commands: &BTreeMap<String, Method>,
    extension_commands: &BTreeMap<&String, Method>,
) {
    let ParseParams {
        module_name,
        lib_name,
        strip_prefix_constant,
        strip_prefix_method,
        prepend,
        output,
        ..
    } = params;

    let write_comment = |prefix: &str, s: &Option<String>, output: &mut O| {
        if let Some(s) = s {
            writeln!(output, "{} {}", prefix, s).unwrap();
        };
    };

    writeln!(output, "#[allow(non_camel_case_types, non_snake_case, dead_code, non_upper_case_globals)]").unwrap();
    writeln!(output, "pub mod {} {{", module_name).unwrap();

    if let Some(prepend) = prepend {
        writeln!(output, "{}", prepend).unwrap();
    };

    writeln!(output, "// {} constants:", constants.len()).unwrap();
    let strip = |s| {
        let stripped = strip_prefix(strip_prefix_constant, s);
        if stripped.chars().nth(0).unwrap().is_ascii_digit() {
            s // cannot start with a number
        } else {
            stripped
        }
    };
    let mut uniques = BTreeSet::new();
    for EnumValue {
        key,
        value,
        comment,
        alias,
    } in constants.iter()
    {
        let is_unique = uniques.insert(key);
        let mut dedup_s = "";
        if is_unique {
            dedup_s = "2";
        }
        write_comment("///", comment, output);
        write_comment("/// alias:", alias, output);
        writeln!(
            output,
            "pub const {}{}: {} = {};",
            strip(key),
            dedup_s,
            make_constant_type(value),
            make_constant_value(value),
        )
        .unwrap();
    }

    writeln!(output, "\n// {} enums:", enums.len()).unwrap();
    for Enums {
        name,
        comment,
        values,
    } in enums
    {
        let mut uniques = BTreeSet::new();
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
            let is_unique = uniques.insert(value.as_str());
            if is_unique {
                write_comment("  ///", comment, output);
                write_comment("  /// alias:", alias, output);
                writeln!(output, "  {} = {},", strip(key), value).unwrap();
            } else {
                write_comment("  //", comment, output);
                write_comment("  // alias:", alias, output);
                writeln!(output, "  // {} = {},", strip(key), value).unwrap();
            }
        }
        writeln!(output, "}}").unwrap();
    }

    writeln!(output, "\n// {} methods:", commands.len()).unwrap();
    writeln!(output, "#[allow(clashing_extern_declarations)]").unwrap();
    writeln!(output, "#[link(name = \"{}\")]", lib_name).unwrap();
    writeln!(output, "extern \"C\" {{").unwrap();
    let strip = |s| strip_prefix(strip_prefix_method, s);
    for Method {
        return_type,
        method_name,
        params,
    } in commands.values()
    {
        write!(output, "pub fn {}(", strip(method_name)).unwrap();
        let params = params
            .iter()
            .map(|(type_name, name)| {
                format!("{}: {}", rename_param(name), convert_param_type(type_name))
            })
            .collect::<Vec<_>>();
        write!(output, "{})", params.join(", ")).unwrap();
        if return_type != "void" {
            writeln!(output, " -> {};", convert_param_type(return_type)).unwrap();
        } else {
            writeln!(output, ";").unwrap();
        }
    }
    writeln!(output, "}}").unwrap();

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
            "pub fn load_{}(loader: impl Fn(&core::ffi::CStr) -> Option<*mut core::ffi::c_void>)",
            method_name
        )
        .unwrap();
        write!(output, "-> Option<unsafe extern \"C\" fn (").unwrap();
        let params = params
            .iter()
            .map(|(type_name, _name)| convert_param_type(type_name.as_str()))
            .collect::<Vec<_>>();
        write!(output, "{}", params.join(", ")).unwrap();
        if return_type == "void" {
            writeln!(output, ")> {{").unwrap();
        } else {
            writeln!(output, ") -> {}> {{", convert_param_type(return_type)).unwrap();
        }
        writeln!(
            output,
            "  loader(c\"{}\").map(|ptr| unsafe {{ std::mem::transmute(ptr) }})",
            method_name
        )
        .unwrap();
        writeln!(output, "}}").unwrap()
    }

    writeln!(output, "}}\n").unwrap();
}

fn parse_xml<O: std::io::Write>(params: ParseParams<O>) {
    let ParseParams {
        source_path: path,
        ..
    } = &params;

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
                    if attrs.get("type").map(|t| t == "bitmask").unwrap_or(false) {
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
        params,
        &enums,
        &constants,
        &commands,
        &extension_commands,
    );
}

struct ParseParams<'a, O> {
    source_path: &'a str,
    module_name: &'a str,
    lib_name: &'a str,
    strip_prefix_constant: &'a str,
    strip_prefix_method: &'a str,
    prepend: Option<&'a str>,
    output: &'a mut O,
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    let mut dest = Vec::with_capacity(2 * 1024 * 1024 * 1024);
    parse_xml(ParseParams {
        source_path: "gl.xml",
        module_name: "gl",
        lib_name: "GLESv2",
        strip_prefix_constant: "GL_",
        strip_prefix_method: "gl",
        prepend: Some(GL_PREPEND_STR),
        output: &mut dest,
    });
    parse_xml(ParseParams {
        source_path: "egl.xml",
        module_name: "egl",
        lib_name: "EGL",
        strip_prefix_constant: "EGL_",
        strip_prefix_method: "egl",
        prepend: Some(EGL_PREPEND_STR),
        output: &mut dest,
    });
    std::fs::write(format!("{}/lib.rs", out_dir), dest).unwrap();
}

const GL_PREPEND_STR: &str = "pub type GLvoid = core::ffi::c_void;
pub type GLbyte = core::ffi::c_char;
pub type GLubyte = core::ffi::c_uchar;
pub type GLchar = core::ffi::c_char;
pub type GLboolean = core::ffi::c_uchar;
pub type GLshort = core::ffi::c_short;
pub type GLushort = core::ffi::c_ushort;
pub type GLint = core::ffi::c_int;
pub type GLuint = core::ffi::c_uint;
pub type GLint64 = i64;
pub type GLuint64 = u64;
pub type GLintptr = isize;
pub type GLsizeiptr = isize;
pub type GLintptrARB = isize;
pub type GLsizeiptrARB = isize;
pub type GLint64EXT = i64;
pub type GLuint64EXT = u64;
pub type GLsizei = GLint;
pub type GLclampx = core::ffi::c_int;
pub type GLfixed = GLint;
pub type GLhalf = core::ffi::c_ushort;
pub type GLhalfNV = core::ffi::c_ushort;
pub type GLhalfARB = core::ffi::c_ushort;
pub type GLenum = core::ffi::c_uint;
pub type GLbitfield = core::ffi::c_uint;
pub type GLfloat = core::ffi::c_float;
pub type GLdouble = core::ffi::c_double;
pub type GLclampf = core::ffi::c_float;
pub type GLclampd = core::ffi::c_double;
pub type GLcharARB = core::ffi::c_char;
pub type GLhandleARB = core::ffi::c_uint;
pub struct __GLsync;
pub type GLsync = *mut __GLsync;
pub type GLeglImageOES = *mut core::ffi::c_void;
pub type GLeglClientBufferEXT = *mut core::ffi::c_void;
pub type GLvdpauSurfaceNV = GLintptr;
pub type GLVULKANPROCNV = extern \"C\" fn();
pub type GLDEBUGPROC = extern \"C\" fn(GLenum, GLenum, GLuint, GLenum, GLsizei, *const GLchar, *const core::ffi::c_void);
pub type GLDEBUGPROCARB = extern \"C\" fn(GLenum, GLenum, GLuint, GLenum, GLsizei, *const GLchar, *const core::ffi::c_void);
pub type GLDEBUGPROCKHR = extern \"C\" fn(GLenum, GLenum, GLuint, GLenum, GLsizei, *const GLchar, *const core::ffi::c_void);
pub type GLDEBUGPROCAMD = extern \"C\" fn(GLuint, GLenum, GLuint, GLenum, GLsizei, *const GLchar, *const core::ffi::c_void);
pub struct _cl_event;
pub struct _cl_context;
";

const EGL_PREPEND_STR: &str = "
pub type EGLint = core::ffi::c_int;
pub struct AHardwareBuffer;
pub struct wl_buffer;
pub struct wl_display;
pub struct wl_resource;
pub type EGLBoolean = core::ffi::c_uint;
pub type EGLenum = core::ffi::c_uint;
pub type EGLAttribKHR = isize;
pub type EGLAttrib = isize;
pub type EGLClientBuffer = *mut core::ffi::c_void;
pub type EGLConfig = *mut core::ffi::c_void;
pub type EGLContext = *mut core::ffi::c_void;
pub type EGLDeviceEXT = *mut core::ffi::c_void;
pub type EGLDisplay = *mut core::ffi::c_void;
pub type EGLImage = *mut core::ffi::c_void;
pub type EGLImageKHR = *mut core::ffi::c_void;
pub type EGLLabelKHR = *mut core::ffi::c_void;
pub type EGLObjectKHR = *mut core::ffi::c_void;
pub type EGLOutputLayerEXT = *mut core::ffi::c_void;
pub type EGLOutputPortEXT = *mut core::ffi::c_void;
pub type EGLStreamKHR = *mut core::ffi::c_void;
pub type EGLSurface = *mut core::ffi::c_void;
pub type EGLSync = *mut core::ffi::c_void;
pub type EGLSyncKHR = *mut core::ffi::c_void;
pub type EGLSyncNV = *mut core::ffi::c_void;
pub type khronos_int8_t  = i8;
pub type khronos_uint8_t = u8;
pub type khronos_int16_t = i16;
pub type khronos_uint16_t = u16;
pub type khronos_int32_t = i32;
pub type khronos_uint32_t = u32;
pub type khronos_int64_t = i64;
pub type khronos_uint64_t = u64;
pub type khronos_intptr_t = isize;
pub type khronos_uintptr_t = usize;
pub type khronos_ssize_t  = isize;
pub type khronos_usize_t  = usize;
pub type khronos_float_t = core::ffi::c_float;
pub type khronos_time_ns_t = u64;
pub type khronos_stime_nanoseconds_t = i64;
pub type khronos_utime_nanoseconds_t = u64;
pub type EGLnsecsANDROID = khronos_stime_nanoseconds_t;
pub type EGLsizeiANDROID = khronos_ssize_t;
pub type EGLTimeKHR = khronos_utime_nanoseconds_t;
pub type EGLTime = khronos_utime_nanoseconds_t;
pub type EGLTimeNV = khronos_utime_nanoseconds_t;
pub type EGLuint64KHR = khronos_uint64_t;
pub type EGLuint64NV = khronos_utime_nanoseconds_t;
pub type EGLNativeFileDescriptorKHR = core::ffi::c_int;
#[repr(C)]
pub struct EGLClientPixmapHI {
    pData: core::ffi::c_void,
    iWidth: EGLint,
    iHeight: EGLint,
    iStride: EGLint,
}
pub type EGLNativeDisplayType = *mut core::ffi::c_void;
pub type EGLNativePixmapType = *mut core::ffi::c_void;
pub type EGLNativeWindowType = *mut core::ffi::c_void;
pub type EGLGetBlobFuncANDROID = extern \"C\" fn (*const core::ffi::c_void, EGLsizeiANDROID, *mut core::ffi::c_void, EGLsizeiANDROID) -> EGLsizeiANDROID;
pub type EGLSetBlobFuncANDROID = extern \"C\" fn (*const core::ffi::c_void, EGLsizeiANDROID, *const core::ffi::c_void, EGLsizeiANDROID);
pub type EGLDEBUGPROCKHR = extern \"C\" fn (EGLenum, *const core::ffi::c_char, EGLint, EGLLabelKHR, EGLLabelKHR, *const core::ffi::c_char);
pub type __eglMustCastToProperFunctionPointerType = *mut core::ffi::c_void;
";
