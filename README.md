# gren-parse

A small library with all the basic building blocks to create combinatory parsers.
It is much simpler than [gren-lang/parser](https://packages.gren-lang.org/package/gren-lang/parser).
Here is a parser for Booleans:

```
import Parse

parseBool =
    Parse.oneOf
        [ Parse.string "false" |> Parse.map ( \ _ -> [ False ] )
        , Parse.string "true" |> Parse.map ( \ _ -> [ True ] )
        ]
```
```
> Parse.run parseBool "true"
Just True : Maybe Bool

> Parse.run parseBool "false"
Just False : Maybe Bool

> Parse.run parseBool "asdf"
Nothing : Maybe Bool
```

Package documentation:
[packages.gren-lang.org/package/aramiscd/gren-parse](https://packages.gren-lang.org/package/aramiscd/gren-parse)

See [aramiscd/gren-json](https://github.com/aramiscd/gren-json) for a JSON parser based on this.

See [https://vimeo.com/171704565](https://vimeo.com/171704565) for an introduction into combinatory parsing.
