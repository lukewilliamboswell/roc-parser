//! Consume preloaded input without retaining completed records or events.
//! These modes are separate from the complete-model collection comparators.

use serde::de::{SeqAccess, Visitor};

pub fn ndjson(text: &str) -> Result<u64, ()> {
    text.split('\n')
        .filter(|line| !line.trim_matches([' ', '\t', '\r']).is_empty())
        .try_fold(0, |sum, line| {
            super::serde_json_parse(line).map(|n| sum + n)
        })
}

pub fn json_array(text: &str) -> Result<u64, ()> {
    struct Items;
    impl<'de> Visitor<'de> for Items {
        type Value = u64;

        fn expecting(&self, out: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            out.write_str("a top-level JSON array")
        }

        fn visit_seq<A: SeqAccess<'de>>(self, mut items: A) -> Result<u64, A::Error> {
            let mut sum = 6;
            while let Some(value) = items.next_element::<serde_json::Value>()? {
                sum += super::json_walk(&value);
            }
            Ok(sum)
        }
    }

    let mut parser = serde_json::Deserializer::from_str(text);
    parser.disable_recursion_limit();
    let sum = serde::Deserializer::deserialize_seq(&mut parser, Items).map_err(|_| ())?;
    parser.end().map_err(|_| ())?;
    Ok(sum)
}

pub fn xml(text: &str) -> Result<u64, ()> {
    use quick_xml::events::Event;
    let mut reader = quick_xml::Reader::from_str(text);
    reader.config_mut().check_comments = true;
    let (mut sum, mut depth, mut roots) = (0, 0usize, 0);
    let mut in_text = false;

    loop {
        match reader.read_event().map_err(|_| ())? {
            event @ (Event::Start(_) | Event::Empty(_)) => {
                let empty = matches!(&event, Event::Empty(_));
                let tag = match event {
                    Event::Start(tag) | Event::Empty(tag) => tag,
                    _ => unreachable!(),
                };
                if depth == 0 {
                    roots += 1;
                    if roots != 1 {
                        return Err(());
                    }
                }
                in_text = false;
                sum += tag.name().as_ref().len() as u64;
                for attr in tag.attributes() {
                    let attr = attr.map_err(|_| ())?;
                    sum += attr.key.as_ref().len() as u64;
                    let raw = std::str::from_utf8(&attr.value).map_err(|_| ())?;
                    let normalized;
                    let source = if raw.bytes().any(|b| matches!(b, b'\r' | b'\n' | b'\t')) {
                        normalized = raw.replace("\r\n", " ").replace(['\r', '\n', '\t'], " ");
                        normalized.as_str()
                    } else {
                        raw
                    };
                    sum += quick_xml::escape::unescape(source).map_err(|_| ())?.len() as u64;
                }
                if !empty {
                    depth += 1;
                }
            }
            Event::End(_) => {
                depth = depth.checked_sub(1).ok_or(())?;
                in_text = false;
            }
            Event::Text(value) if depth != 0 => {
                let value = value.xml10_content().map_err(|_| ())?;
                if !value.is_empty() {
                    sum += value.len() as u64 + u64::from(!in_text);
                    in_text = true;
                }
            }
            Event::CData(value) if depth != 0 => {
                let value = value.xml10_content().map_err(|_| ())?;
                if !value.is_empty() {
                    sum += value.len() as u64 + u64::from(!in_text);
                    in_text = true;
                }
            }
            Event::GeneralRef(value) if depth != 0 => {
                let size = match value.resolve_char_ref().map_err(|_| ())? {
                    Some(ch) => ch.len_utf8(),
                    None => match value.as_ref() {
                        b"amp" | b"lt" | b"gt" | b"quot" | b"apos" => 1,
                        _ => return Err(()),
                    },
                };
                sum += size as u64 + u64::from(!in_text);
                in_text = true;
            }
            Event::Text(value) => {
                if !value
                    .as_ref()
                    .iter()
                    .all(|byte| matches!(byte, b' ' | b'\t' | b'\r' | b'\n'))
                {
                    return Err(());
                }
            }
            Event::DocType(_) | Event::CData(_) | Event::GeneralRef(_) => return Err(()),
            Event::Eof => {
                return if depth == 0 && roots == 1 {
                    Ok(sum)
                } else {
                    Err(())
                }
            }
            _ => {}
        }
    }
}

// httparse borrows headers while this consumer discards each completed head.
// Body/trailer framing and checksum traversal are included; source I/O is not.
pub fn http(text: &str) -> Result<u64, ()> {
    let mut rest = text.as_bytes();
    let mut sum = 0;
    while !rest.is_empty() {
        let mut storage = [httparse::EMPTY_HEADER; 128];
        let mut switched = false;
        let (head_len, fields, no_body, close_delimited, head_sum) = if rest.starts_with(b"HTTP/") {
            let mut head = httparse::Response::new(&mut storage);
            let httparse::Status::Complete(n) = head.parse(rest).map_err(|_| ())? else {
                return Err(());
            };
            let code = head.code.ok_or(())?;
            switched = code == 101;
            let head_sum = head.headers.len() as u64 + u64::from(code);
            (
                n,
                head.headers,
                code < 200 || code == 204 || code == 304,
                true,
                head_sum,
            )
        } else {
            let mut head = httparse::Request::new(&mut storage);
            let httparse::Status::Complete(n) = head.parse(rest).map_err(|_| ())? else {
                return Err(());
            };
            let head_sum = head.headers.len() as u64 + head.path.ok_or(())?.len() as u64;
            (n, head.headers, false, false, head_sum)
        };
        sum += head_sum;
        let mut length = None;
        let mut chunked = false;
        for field in fields {
            if field.name.eq_ignore_ascii_case("content-length") {
                for item in field.value.split(|byte| *byte == b',') {
                    let item = std::str::from_utf8(item)
                        .map_err(|_| ())?
                        .trim_matches([' ', '\t']);
                    if item.is_empty() || !item.bytes().all(|b| b.is_ascii_digit()) {
                        return Err(());
                    }
                    let n = item.parse::<usize>().map_err(|_| ())?;
                    if length.is_some_and(|old| old != n) {
                        return Err(());
                    }
                    length = Some(n);
                }
            } else if field.name.eq_ignore_ascii_case("transfer-encoding") {
                if chunked || !field.value.eq_ignore_ascii_case(b"chunked") {
                    return Err(());
                }
                chunked = true;
            }
        }
        if chunked && length.is_some() {
            return Err(());
        }
        rest = &rest[head_len..];
        if switched {
            return Ok(sum + rest.len() as u64);
        }
        if no_body {
            continue;
        }
        if chunked {
            loop {
                let httparse::Status::Complete((n, count)) =
                    httparse::parse_chunk_size(rest).map_err(|_| ())?
                else {
                    return Err(());
                };
                rest = &rest[n..];
                let count = usize::try_from(count).map_err(|_| ())?;
                if count == 0 {
                    let mut trailers = [httparse::EMPTY_HEADER; 128];
                    let httparse::Status::Complete((n, trailers)) =
                        httparse::parse_headers(rest, &mut trailers).map_err(|_| ())?
                    else {
                        return Err(());
                    };
                    sum += trailers
                        .iter()
                        .map(|h| (h.name.len() + h.value.len()) as u64)
                        .sum::<u64>();
                    rest = &rest[n..];
                    break;
                }
                if rest.get(count..count.checked_add(2).ok_or(())?) != Some(b"\r\n") {
                    return Err(());
                }
                sum += count as u64;
                rest = &rest[count + 2..];
            }
        } else if let Some(count) = length {
            rest = rest.get(count..).ok_or(())?;
            sum += count as u64;
        } else if close_delimited {
            sum += rest.len() as u64;
            rest = &[];
        }
    }
    Ok(sum)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_records_match_complete_values_and_require_completion() {
        assert_eq!(
            json_array("[1,{\"x\":true},\"é\"]"),
            super::super::serde_json_parse("[1,{\"x\":true},\"é\"]")
        );
        assert_eq!(ndjson("1\r\n\ntrue\n"), Ok(6));
        assert!(json_array("[1]x").is_err());
        assert!(json_array("[1").is_err());
        assert!(ndjson("1\nfalse x").is_err());
    }

    #[test]
    fn xml_text_coalesces_across_references_comments_and_cdata() {
        assert_eq!(
            xml("<a>x&amp;<![CDATA[y\r\nz]]><!--c-->&#x1f600;</a>"),
            Ok(11)
        );
        assert_eq!(xml("<a><b/></a>"), Ok(2));
        assert_eq!(xml("<a x='x\r\ny'/>"), Ok(5));
        for text in [
            "<a>",
            "<a/><b/>",
            "<a>&unknown;</a>",
            "<a><!--bad--comment--></a>",
        ] {
            assert!(xml(text).is_err(), "{text}");
        }
    }

    #[test]
    fn http_pipelines_chunked_trailers_and_truncation() {
        assert_eq!(
            http("GET / HTTP/1.1\r\nHost: a\r\n\r\nGET /x HTTP/1.1\r\nHost: a\r\n\r\n"),
            Ok(5)
        );
        assert_eq!(http("POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n2\r\nhi\r\n0\r\nX: y\r\n\r\n"), Ok(7));
        assert!(http("POST / HTTP/1.1\r\nContent-Length: 2\r\n\r\na").is_err());
        assert!(http("POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n2\r\nhiXX").is_err());
        assert_eq!(
            http("POST / HTTP/1.1\r\nContent-Length: 2, 2\r\n\r\nhi"),
            Ok(4)
        );
        assert_eq!(http("HTTP/1.1 101 Switch\r\n\r\nraw"), Ok(104));
    }
}
