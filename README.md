# gren-parse

Basic building blocks for the construction of combinatory parsers


## Example

Here is a parser for booleans


```
import StringParser as Parse

parseBool =
    Parse.oneOf
        [ Parse.string "false" |> Parse.map ( \ _ -> [ False ] )
        , Parse.string "true" |> Parse.map ( \ _ -> [ True ] )
        ]
```
```
> Parse.run parseBool "true"
Just True

> Parse.run parseBool "false"
Just False

> Parse.run parseBool "asdf"
Nothing
```


## Usage

Use `StringParser` to construct parsers that operate directly on `String` input.

Advanced parsers first break down their input into a sequence of tokens.
This is called tokenization.
You can use `StringParser` for this tokenization step.
Then use `ArrayParser` to further process the tokenized input.

Don't worry about tokenization if you're not sure you need it.
Being able to omit tokenization is a great advantage in combinatory parsing.
So start with `StringParser` if you are new to parser construction.
This will get you a long way.
I have written a complete and correct but somewhat inefficient JSON parser this way.
Sooner or later you will probably realize by yourself that you have to tokenize for certain parsing challenges.
I realized that I needed this when I wanted to improve the efficiency of my JSON parser.


## Documentation

[packages.gren-lang.org/package/aramiscd/gren-parse](https://packages.gren-lang.org/package/aramiscd/gren-parse)

---

See [aramiscd/gren-json](https://github.com/aramiscd/gren-json) for a JSON parser based on this.

See [https://vimeo.com/171704565](https://vimeo.com/171704565) for an introduction to combinatory parsing.
