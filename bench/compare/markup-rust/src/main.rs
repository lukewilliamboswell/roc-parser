use html5ever::{local_name, ns, parse_document, parse_fragment, tendril::TendrilSink, QualName};
use markup5ever_rcdom::{Handle, NodeData, RcDom};
use serde_json::{json, Value};
use std::{hint::black_box, io::Read, time::Instant};

fn roxmltree(text: &str) -> u64 {
    let doc = roxmltree::Document::parse(text).expect("valid XML");
    doc.descendants()
        .map(|n| {
            if n.is_element() {
                1 + n.tag_name().name().len() as u64
                    + n.attributes()
                        .map(|a| 1 + (a.name().len() + a.value().len()) as u64)
                        .sum::<u64>()
            } else if n.is_text() {
                1 + n.text().unwrap_or("").len() as u64
            } else {
                0
            }
        })
        .sum()
}

enum Node {
    Element(String, String, Vec<(String, String, String)>, Vec<Node>),
    Text(String),
}
fn walk(n: &Node) -> u64 {
    match n {
        Node::Text(t) => 1 + t.len() as u64,
        Node::Element(_, name, attrs, children) => {
            1 + name.len() as u64
                + attrs
                    .iter()
                    .map(|(_, k, v)| 1 + (k.len() + v.len()) as u64)
                    .sum::<u64>()
                + children.iter().map(walk).sum::<u64>()
        }
    }
}
fn push(stack: &mut [Node], node: Node) {
    if let Some(Node::Element(_, _, _, children)) = stack.last_mut() {
        if let Node::Text(t) = &node {
            if let Some(Node::Text(old)) = children.last_mut() {
                old.push_str(t);
                return;
            }
        }
        children.push(node);
    }
}
fn quick_xml_tree(text: &str) -> Vec<Node> {
    use quick_xml::events::Event;
    let mut reader = quick_xml::NsReader::from_str(text);
    let mut stack = vec![Node::Element(String::new(), String::new(), vec![], vec![])];
    fn uri(value: quick_xml::name::ResolveResult) -> String {
        match value {
            quick_xml::name::ResolveResult::Bound(ns) => ns.as_ref().to_owned(),
            quick_xml::name::ResolveResult::Unbound => String::new(),
            quick_xml::name::ResolveResult::Unknown(p) => panic!("unbound prefix {p}"),
        }
    }
    fn element(
        e: &quick_xml::events::BytesStart,
        resolver: &quick_xml::name::NamespaceResolver,
    ) -> Node {
        let (namespace, local) = resolver.resolve_element(e.name());
        let name = local.as_ref().to_owned();
        let namespace = uri(namespace);
        let attrs = e
            .attributes()
            .map(|a| a.unwrap())
            .filter(|a| a.key.as_ref() != "xmlns" && !a.key.as_ref().starts_with("xmlns:"))
            .map(|a| {
                let (space, local) = resolver.resolve_attribute(a.key);
                (
                    uri(space),
                    local.as_ref().to_owned(),
                    a.normalized_value(quick_xml::XmlVersion::Implicit1_0)
                        .unwrap()
                        .into_owned(),
                )
            })
            .collect();
        Node::Element(namespace, name, attrs, vec![])
    }
    loop {
        match reader.read_event().unwrap() {
            Event::Start(e) => stack.push(element(&e, reader.resolver())),
            Event::Empty(e) => push(&mut stack, element(&e, reader.resolver())),
            Event::End(_) => {
                let n = stack.pop().unwrap();
                push(&mut stack, n);
            }
            Event::Text(t) => push(
                &mut stack,
                Node::Text(
                    t.xml_content(quick_xml::XmlVersion::Implicit1_0)
                        .into_owned(),
                ),
            ),
            Event::CData(t) => push(
                &mut stack,
                Node::Text(
                    t.xml_content(quick_xml::XmlVersion::Implicit1_0)
                        .into_owned(),
                ),
            ),
            Event::GeneralRef(r) => {
                let name = r.as_ref();
                let value = match name {
                    "amp" => "&".to_owned(),
                    "lt" => "<".to_owned(),
                    "gt" => ">".to_owned(),
                    "apos" => "'".to_owned(),
                    "quot" => "\"".to_owned(),
                    _ => {
                        let number = if let Some(hex) = name.strip_prefix("#x") {
                            u32::from_str_radix(hex, 16).unwrap()
                        } else {
                            name.strip_prefix('#').unwrap().parse().unwrap()
                        };
                        char::from_u32(number).unwrap().to_string()
                    }
                };
                push(&mut stack, Node::Text(value));
            }
            Event::Eof => break,
            _ => {}
        }
    }
    assert_eq!(stack.len(), 1);
    match stack.pop().unwrap() {
        Node::Element(_, _, _, children) => children,
        _ => unreachable!(),
    }
}
fn quick_xml(text: &str) -> u64 {
    quick_xml_tree(text).iter().map(walk).sum()
}
fn node_json(n: &Node) -> Value {
    match n {
        Node::Text(t) => json!(["text", t]),
        Node::Element(ns, name, attrs, children) => json!([
            "element",
            ns,
            name,
            attrs,
            children.iter().map(node_json).collect::<Vec<_>>()
        ]),
    }
}
fn roxml_json(n: roxmltree::Node) -> Option<Value> {
    if n.is_element() {
        Some(json!([
            "element",
            n.tag_name().namespace().unwrap_or(""),
            n.tag_name().name(),
            n.attributes()
                .map(|a| json!([a.namespace().unwrap_or(""), a.name(), a.value()]))
                .collect::<Vec<_>>(),
            n.children().filter_map(roxml_json).collect::<Vec<_>>()
        ]))
    } else if n.is_text() {
        Some(json!(["text", n.text().unwrap_or("")]))
    } else {
        None
    }
}
fn html_json(n: &Handle) -> Option<Value> {
    match &n.data {
        NodeData::Element { name, attrs, .. } => Some(json!([
            "element",
            name.ns.to_string(),
            name.local.to_string(),
            attrs
                .borrow()
                .iter()
                .filter(|a| a.name.ns != ns!(xmlns))
                .map(|a| json!([
                    a.name.ns.to_string(),
                    a.name.local.to_string(),
                    a.value.to_string()
                ]))
                .collect::<Vec<_>>(),
            n.children
                .borrow()
                .iter()
                .filter_map(html_json)
                .collect::<Vec<_>>()
        ])),
        NodeData::Text { contents } => Some(json!(["text", contents.borrow().as_ref()])),
        _ => None,
    }
}
fn html_dom(text: &str, fragment: bool) -> RcDom {
    if fragment {
        parse_fragment(
            RcDom::default(),
            Default::default(),
            QualName::new(None, ns!(html), local_name!("body")),
            vec![],
            false,
        )
        .one(text)
    } else {
        parse_document(RcDom::default(), Default::default()).one(text)
    }
}
fn canonical(text: &str, implementation: &str) -> Value {
    match implementation {
        "roxmltree" => json!(roxmltree::Document::parse(text)
            .unwrap()
            .root()
            .children()
            .filter_map(roxml_json)
            .collect::<Vec<_>>()),
        "quick-xml" => json!(quick_xml_tree(text)
            .iter()
            .map(node_json)
            .collect::<Vec<_>>()),
        _ => {
            let fragment = implementation == "html-fragment";
            let doc = html_dom(text, fragment);
            let roots = if fragment {
                doc.document.children.borrow()[0]
                    .children
                    .borrow()
                    .iter()
                    .filter_map(html_json)
                    .collect::<Vec<_>>()
            } else {
                doc.document
                    .children
                    .borrow()
                    .iter()
                    .filter_map(html_json)
                    .collect::<Vec<_>>()
            };
            json!(roots)
        }
    }
}
fn html_walk(node: &Handle) -> u64 {
    let weight = match &node.data {
        NodeData::Element { name, attrs, .. } => {
            1 + name.local.len() as u64
                + attrs
                    .borrow()
                    .iter()
                    .filter(|a| a.name.ns != ns!(xmlns))
                    .map(|a| 1 + (a.name.local.len() + a.value.len()) as u64)
                    .sum::<u64>()
        }
        NodeData::Text { contents } => 1 + contents.borrow().len() as u64,
        _ => 0,
    };
    weight + node.children.borrow().iter().map(html_walk).sum::<u64>()
}
fn html(text: &str, fragment: bool) -> u64 {
    let doc = html_dom(text, fragment);
    if fragment {
        doc.document.children.borrow()[0]
            .children
            .borrow()
            .iter()
            .map(html_walk)
            .sum()
    } else {
        html_walk(&doc.document)
    }
}
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let count: u64 = args[2].parse().unwrap();
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    if count == 0 {
        println!("{}", canonical(&input, &args[1]));
        return;
    }
    let mut checksum = 0u64;
    let start = Instant::now();
    for _ in 0..count {
        checksum = checksum.wrapping_add(black_box(match args[1].as_str() {
            "roxmltree" => roxmltree(black_box(&input)),
            "quick-xml" => quick_xml(black_box(&input)),
            "html-document" => html(black_box(&input), false),
            "html-fragment" => html(black_box(&input), true),
            _ => panic!("unknown adapter"),
        }));
    }
    let elapsed = start.elapsed().as_nanos().max(1);
    println!("{{\"iterations\":{count},\"elapsed_ns\":{elapsed},\"successes\":{count},\"checksum\":{checksum}}}");
}
