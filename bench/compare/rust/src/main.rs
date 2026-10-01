//! Rust comparators for scripts/bench.py.
//!
//! Usage: compare <impl> <iterations> < document
//!
//! Same protocol as the Roc drivers in bench/roc/: the document is read from
//! stdin before the clock starts, parsed <iterations> times, and one JSON line
//! is printed. Every implementation builds owned values (Strings, Vecs or the
//! library's own tree) so the work matches the trees roc-parser returns;
//! quick-xml and pulldown-cmark are streaming parsers, so their events are
//! consumed and their text copied into owned values.
use std::hint::black_box;
use std::io::Read;
use std::time::Instant;

type Parse = fn(&str) -> Result<u64, ()>;

fn csv_parse(text: &str) -> Result<u64, ()> {
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(false)
        .flexible(true)
        .from_reader(text.as_bytes());
    let mut rows: Vec<Vec<String>> = Vec::new();
    for record in reader.records() {
        let record = record.map_err(|_| ())?;
        rows.push(record.iter().map(str::to_owned).collect());
    }
    Ok(rows.iter().map(|r| 1 + r.iter().map(|f| f.len() as u64).sum::<u64>()).sum())
}

fn yaml_walk(value: &yaml_rust2::Yaml) -> u64 {
    use yaml_rust2::Yaml;
    match value {
        Yaml::Hash(map) => 7 + map.iter().map(|(k, v)| yaml_walk(k) + yaml_walk(v)).sum::<u64>(),
        Yaml::Array(items) => 6 + items.iter().map(yaml_walk).sum::<u64>(),
        Yaml::String(s) => 5 + s.len() as u64,
        _ => 1,
    }
}

fn yaml_rust2_parse(text: &str) -> Result<u64, ()> {
    let docs = yaml_rust2::YamlLoader::load_from_str(text).map_err(|_| ())?;
    Ok(docs.iter().map(yaml_walk).sum())
}

fn serde_yaml_walk(value: &serde_yaml::Value) -> u64 {
    use serde_yaml::Value;
    match value {
        Value::Mapping(map) => 7 + map.iter().map(|(k, v)| serde_yaml_walk(k) + serde_yaml_walk(v)).sum::<u64>(),
        Value::Sequence(items) => 6 + items.iter().map(serde_yaml_walk).sum::<u64>(),
        Value::String(s) => 5 + s.len() as u64,
        Value::Tagged(t) => serde_yaml_walk(&t.value),
        _ => 1,
    }
}

fn serde_yaml_parse(text: &str) -> Result<u64, ()> {
    let value: serde_yaml::Value = serde_yaml::from_str(text).map_err(|_| ())?;
    Ok(serde_yaml_walk(&value))
}

enum Node {
    Element(String, Vec<(String, String)>, Vec<Node>),
    Text(String),
}

fn node_walk(node: &Node) -> u64 {
    match node {
        Node::Text(t) => 1 + t.len() as u64,
        Node::Element(name, attrs, children) => {
            name.len() as u64
                + attrs.iter().map(|(k, v)| (k.len() + v.len()) as u64).sum::<u64>()
                + children.iter().map(node_walk).sum::<u64>()
        }
    }
}

/// quick-xml is a pull parser; this builds the same element/text tree that
/// Xml.parse_str returns, unescaping text and attribute values.
fn quick_xml_parse(text: &str) -> Result<u64, ()> {
    use quick_xml::events::Event;
    let mut reader = quick_xml::Reader::from_str(text);
    let mut stack: Vec<Node> = vec![Node::Element(String::new(), Vec::new(), Vec::new())];
    fn start(e: &quick_xml::events::BytesStart) -> Result<Node, ()> {
        let name = std::str::from_utf8(e.name().as_ref()).map_err(|_| ())?.to_owned();
        let mut attrs = Vec::new();
        for a in e.attributes() {
            let a = a.map_err(|_| ())?;
            let key = std::str::from_utf8(a.key.as_ref()).map_err(|_| ())?.to_owned();
            let value = a.unescape_value().map_err(|_| ())?.into_owned();
            attrs.push((key, value));
        }
        Ok(Node::Element(name, attrs, Vec::new()))
    }
    fn push(stack: &mut [Node], node: Node) {
        if let Some(Node::Element(_, _, children)) = stack.last_mut() {
            children.push(node);
        }
    }
    loop {
        match reader.read_event().map_err(|_| ())? {
            Event::Start(e) => stack.push(start(&e)?),
            Event::Empty(e) => {
                let node = start(&e)?;
                push(&mut stack, node);
            }
            Event::End(_) => {
                let node = stack.pop().ok_or(())?;
                push(&mut stack, node);
            }
            Event::Text(t) => {
                let s = t.decode().map_err(|_| ())?.into_owned();
                push(&mut stack, Node::Text(s));
            }
            Event::GeneralRef(r) => {
                let s = std::str::from_utf8(r.as_ref()).map_err(|_| ())?.to_owned();
                push(&mut stack, Node::Text(s));
            }
            Event::CData(c) => {
                let s = std::str::from_utf8(c.as_ref()).map_err(|_| ())?.to_owned();
                push(&mut stack, Node::Text(s));
            }
            Event::Eof => break,
            _ => {}
        }
    }
    if stack.len() != 1 {
        return Err(());
    }
    Ok(stack.iter().map(node_walk).sum())
}

fn roxmltree_parse(text: &str) -> Result<u64, ()> {
    let doc = roxmltree::Document::parse(text).map_err(|_| ())?;
    Ok(doc
        .descendants()
        .map(|n| {
            n.tag_name().name().len() as u64
                + n.text().map_or(0, |t| t.len() as u64)
                + n.attributes().map(|a| (a.name().len() + a.value().len()) as u64).sum::<u64>()
        })
        .sum())
}

/// pulldown-cmark is a pull parser; owning every event matches the cost of
/// keeping a tree without building one.
fn pulldown_cmark_parse(text: &str) -> Result<u64, ()> {
    use pulldown_cmark::{Options, Parser};
    let options = Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH | Options::ENABLE_TASKLISTS;
    let events: Vec<pulldown_cmark::Event<'static>> = Parser::new_ext(text, options).map(|e| e.into_static()).collect();
    Ok(events.len() as u64 + 1)
}

fn comrak_parse(text: &str) -> Result<u64, ()> {
    let arena = comrak::Arena::new();
    let mut options = comrak::Options::default();
    options.extension.table = true;
    options.extension.strikethrough = true;
    options.extension.tasklist = true;
    let root = comrak::parse_document(&arena, text, &options);
    Ok(root.children().count() as u64 + 1)
}

/// httparse only parses the head and borrows from the input; the body is
/// framed here (Content-Length or chunked via httparse's chunk-size parser)
/// and copied, and header values are copied, to match HTTP.request/response.
fn httparse_parse(text: &str) -> Result<u64, ()> {
    let bytes = text.as_bytes();
    let mut headers = [httparse::EMPTY_HEADER; 128];
    let (head_len, owned): (usize, Vec<(String, Vec<u8>)>) = if bytes.starts_with(b"HTTP/") {
        let mut res = httparse::Response::new(&mut headers);
        match res.parse(bytes).map_err(|_| ())? {
            httparse::Status::Complete(n) => (n, res.headers.iter().map(|h| (h.name.to_owned(), h.value.to_vec())).collect()),
            httparse::Status::Partial => return Err(()),
        }
    } else {
        let mut req = httparse::Request::new(&mut headers);
        match req.parse(bytes).map_err(|_| ())? {
            httparse::Status::Complete(n) => (n, req.headers.iter().map(|h| (h.name.to_owned(), h.value.to_vec())).collect()),
            httparse::Status::Partial => return Err(()),
        }
    };
    let mut rest = &bytes[head_len..];
    let mut body = Vec::new();
    let chunked = owned.iter().any(|(k, v)| k.eq_ignore_ascii_case("transfer-encoding") && v.eq_ignore_ascii_case(b"chunked"));
    if chunked {
        loop {
            match httparse::parse_chunk_size(rest).map_err(|_| ())? {
                httparse::Status::Complete((n, size)) => {
                    rest = &rest[n..];
                    if size == 0 {
                        break;
                    }
                    let size = size as usize;
                    if rest.len() < size + 2 {
                        return Err(());
                    }
                    body.extend_from_slice(&rest[..size]);
                    rest = &rest[size + 2..];
                }
                httparse::Status::Partial => return Err(()),
            }
        }
    } else if let Some((_, v)) = owned.iter().find(|(k, _)| k.eq_ignore_ascii_case("content-length")) {
        let len: usize = std::str::from_utf8(v).map_err(|_| ())?.trim().parse().map_err(|_| ())?;
        if rest.len() < len {
            return Err(());
        }
        body.extend_from_slice(&rest[..len]);
    } else {
        body.extend_from_slice(rest);
    }
    Ok(owned.len() as u64 + body.len() as u64 + 1)
}

fn implementation(name: &str) -> Parse {
    match name {
        "csv" => csv_parse,
        "yaml-rust2" => yaml_rust2_parse,
        "serde_yaml" => serde_yaml_parse,
        "quick-xml" => quick_xml_parse,
        "roxmltree" => roxmltree_parse,
        "pulldown-cmark" => pulldown_cmark_parse,
        "comrak" => comrak_parse,
        "httparse" => httparse_parse,
        other => {
            eprintln!("unknown implementation {other}");
            std::process::exit(2)
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let parse = implementation(&args[1]);
    let iterations: u64 = args[2].parse().expect("iterations");
    let mut input = Vec::new();
    std::io::stdin().read_to_end(&mut input).expect("stdin");
    let text = String::from_utf8(input).expect("UTF-8 input");
    let (mut checksum, mut successes) = (0u64, 0u64);
    let start = Instant::now();
    for _ in 0..iterations {
        match parse(black_box(&text)) {
            Ok(sum) => {
                checksum = checksum.wrapping_add(sum);
                successes += 1;
            }
            Err(()) => checksum = checksum.wrapping_add(1),
        }
    }
    let elapsed = start.elapsed().as_nanos().max(1);
    println!("{{\"iterations\":{iterations},\"elapsed_ns\":{elapsed},\"successes\":{successes},\"checksum\":{checksum}}}");
}
