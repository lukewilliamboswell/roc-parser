app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        parser: "../package/main.roc",
    }
    imports [
        cli.Stdout,
        parser.String.{ parseStr },
        parser.Markdown.{ Markdown },
    ]
    provides [main] to cli

content : Str
content =
    """
    # Title

    This is some text

    [roc website](https://roc-lang.org)

    ## Sub-title

    ```roc
    # some code
    foo = bar
    ```
    """

main =
    parseStr Markdown.all content
    |> Result.map \nodes -> renderContent "" nodes
    |> Result.withDefault "PARSING ERROR"
    |> Stdout.line

renderContent : Str, List Markdown -> Str
renderContent = \acc, nodes ->
    when nodes is
        [] -> acc
        [Heading level str, .. as rest] -> Str.concat acc "HEADING: $(Inspect.toStr level) $(str)\n" |> renderContent rest
        [Link { alt, href }, .. as rest] -> Str.concat acc "LINK: $(Inspect.toStr { alt, href })\n" |> renderContent rest
        [Image { alt, href }, .. as rest] -> Str.concat acc "IMAGE: $(Inspect.toStr { alt, href })\n" |> renderContent rest
        [Code { ext, pre }, .. as rest] -> Str.concat acc "CODE: $(Inspect.toStr { ext, pre })\n" |> renderContent rest
        [TODO line, .. as rest] -> Str.concat acc "TODO: $(line)\n" |> renderContent rest
