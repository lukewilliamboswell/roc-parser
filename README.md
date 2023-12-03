# A Parser for Roc

A simple [Parser Combinator](https://en.wikipedia.org/wiki/Parser_combinator) package for Roc.

```roc
color : Parser Utf8 [Red, Green, Blue]
color = 
    oneOf [
        const Red |> skip (string "red"), 
        const Green |> skip (string "green"),
        const Blue |> skip (string "blue"),
    ]

expect parseStr color "green" == Ok Green
```

## Documentation 

See [lukewilliamboswell.github.io/roc-parser/](https://lukewilliamboswell.github.io/roc-parser/)

Locally generate docs using `roc docs package/main.roc` 

## Contributing

If you see anything that could be improved please create an Issue or submit a PR.

## Tests

Run tests locally with `roc test package/main.roc`

## Packaging

Bundle package into a URL for distribution using `roc build --bundle .tar.br package/main.roc` 