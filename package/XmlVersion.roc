XmlVersion :: {
    after_dot : U8,
}.{
    new : U8 -> XmlVersion
    new = |after_dot| { { after_dot } }
}
