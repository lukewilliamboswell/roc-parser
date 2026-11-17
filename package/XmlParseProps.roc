import XmlCore

## Complete XML parsing preserves the source-bounded default; explicit limits are opt-in.
XmlParseProps := { limits : [Unbounded, Bounded(XmlCore.ReaderProps)] ?? Unbounded }
