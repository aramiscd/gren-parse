{-|
    Copyright   : (c) 2019-2023 Aramís Concepción Durán
    License     : GPL-3.0-only
    Maintainer  : Aramís Concepción Durán <aramis@systemli.org>

    = Ein einfaches Parser Combinator Modul

    Dieser Code war ein Teil meines ersten erfolgreichen Versuches, einen nicht völlig trivialen Parser zu
    konstruieren.

    Er basiert auf diesem Vortrag: https://vimeo.com/171704565

    Parser Combinators sind erstaunlich zugänglich.  Ich habe seitdem eine ganze Reihe von Parsern auf der Basis
    dieses Moduls geschrieben.
-}
module Ulme.Parse
    ( Partial ( Partial , value , backlog )
    , Parsed ( Fail , Parsed )
    , Parser
    , parse
    , string
    , fail
    , succeed
    , eitherOr
    , oneOf
    , skip
    , optional
    , pair
    , sequence
    , oneOrMore
    , zeroOrMore
    , map
    )
where

import Ulme hiding ( map , sequence )

import Data.Monoid ( mempty )
import Ulme.List qualified as List
import Ulme.String qualified as String


{-| Datentyp für Zwischenergebnisse beim Parsen.

Felder:

> value     -- der bisher geparste Wert
> backlog   -- die bisher noch nicht gelesenen Eingabezeichen
-}
data Partial a = Partial { value :: a , backlog :: String } deriving ( Show )


{-| Datentyp für Resultate beim Parsen.

Konstruktoren:

> Fail                  -- Parsing gescheitert
> Parsed ( Partial a )  -- Parsing erfolgreich mit einem Wert vom Typ `Partial a`
-}
data Parsed a = Fail | Parsed ( Partial a ) deriving ( Show )


{-| Datentyp für Parser.
-}
type Parser a = String -> Parsed a


{-| Wende einen Parser auf eine Eingabe an.

Der Parser muss die Eingabe vollständig konsumieren und erfolgreich verarbeiten.

>>> parse fooParser "foo"
Just "foo"

>>> parse fooParser "foox"
Nothing
-}
parse :: Parser a -> String -> Maybe a
parse parser input =
    case parser input of
        Parsed partial -> if partial.backlog == "" then Just partial.value else Nothing
        Fail -> Nothing


{-| Ein Parser für Strings.

>>> ( string "abc" ) "abcdef"
Parsed ( Partial { value = "abc" , backlog = "def" } )
-}
string :: String -> String -> Parsed String
string match input =
    case String.uncons match of
        Nothing -> Parsed ( Partial { value = match , backlog = input } )
        Just ( pHead , pTail ) ->
            case String.uncons input of
                Nothing -> Fail
                Just ( sHead , sTail ) ->
                    if pHead /= sHead
                    then Fail
                    else case string pTail sTail of
                        Fail -> Fail
                        Parsed partial ->
                            Parsed
                                ( Partial
                                    { value = String.cons pHead partial.value
                                    , backlog = partial.backlog
                                    }
                                )


{-| Ein Parser, der immer scheitert.

>>> fail "abcdef"
Fail
-}
fail :: Parser a
fail _input = Fail


{-| Ein Parser, der immer erfolgreich einen bestimmten Wert liefert.

>>> ( succeed 321 ) "abcdef"
Parsed ( Partial { value = 321 , backlog = "abcdef" } )
-}
succeed :: a -> Parser a
succeed value input = Parsed ( Partial { value = value , backlog = input } )


{-| Probiere zwei Parser nacheinander aus.

>>> ( eitherOr ( string "x" ) ( string "y" ) ) "yak"
Parsed ( Partial { value = "y" , backlog = "ak" } )
-}
eitherOr :: Parser a -> Parser a -> Parser a
eitherOr parserA parserB input =
    case parserA input of
        Parsed partial -> Parsed partial
        Fail -> parserB input


{-| Probiere mehrere Parser nacheinander aus.

>>> ( oneOf [ string "x" , string "y" , string "z" ] ) "yak"
Parsed ( Partial { value = "y" , backlog = "ak" } )
-}
oneOf :: List ( Parser a ) -> Parser a
oneOf parsers = List.foldr eitherOr fail parsers


{-| Wende einen Parser an und verwirf das Ergebnis.

>>> ( skip ( string "abc" ) ) "abcdef"
Parsed ( Partial { value = "" , backlog = "def" } )
-}
skip :: Monoid a => Parser a -> Parser a
skip parser input =
    case parser input of
        Parsed partial -> Parsed ( Partial { value = mempty , backlog = partial.backlog } )
        Fail -> Fail


{-| Wende einen Parser an, wenn möglich.

>>> ( optional ( string "x" ) ) "xyz"
Parsed ( Partial { value = "x" , backlog = "yz" } )

>>> ( optional ( string "x" ) ) "abc"
Parsed ( Partial { value = "" , backlog = "abc" } )
-}
optional :: Monoid a => Parser a -> Parser a
optional parser input =
    case parser input of
        Parsed partial -> Parsed partial
        Fail -> Parsed ( Partial { value = mempty , backlog = input } )


{-| Verkette zwei Parser.

>>> ( pair ( string "x" ) ( string "y" ) ) "xyz"
Parsed ( Partial { value = "xy" , backlog = "z" } )
-}
pair :: Semigroup a => Parser a -> Parser a -> Parser a
pair parserA parserB input =
    case parserA input of
        Fail -> Fail
        Parsed partialA ->
            case parserB partialA.backlog of
                Fail -> Fail
                Parsed partialB ->
                    Parsed
                        ( Partial
                            { value = partialA.value ++ partialB.value
                            , backlog = partialB.backlog
                            }
                        )


{-| Verkette mehrere Parser.

>>> ( sequence [ string "x" , string "y" , string "z" ] ) "xyz"
Parsed ( Partial { value = "xyz" , backlog = "" } )
-}
sequence :: Monoid a => List ( Parser a ) -> Parser a
sequence parsers = List.foldr pair ( succeed mempty ) parsers


{-| Wende einen Parser mindestens einmal und so oft wie möglich an.

>>> ( oneOrMore ( string "x" ) ) "xxxxa"
Parsed ( Partial { value = "xxxx" , backlog = "a" } )

>>> ( oneOrMore ( string "x" ) ) "aaaaa"
Fail
-}
oneOrMore :: Monoid a => Parser a -> Parser a
oneOrMore parser = sequence [ parser , optional ( oneOrMore parser ) ]


{-| Wende einen Parser so oft wie möglich an.

>>> ( zeroOrMore ( string "x" ) ) "xxxxa"
Parsed ( Partial { value = "xxxx" , backlog = "a" } )

>>> ( zerorMore ( string "x" ) ) "aaaaa"
Parsed ( Partial { value = "" , backlog = "aaaaa" } )
-}
zeroOrMore :: Monoid a => Parser a -> Parser a
zeroOrMore parser = optional ( oneOrMore parser )


{-| Transformiere den geparsten Wert.

>>> import Ulme.Parse qualified as Parse
>>> ( Parse.map String.reverse ( string "abc" ) ) "abcdef"
Parsed ( Partial { value = "cba" , backlog = "def" } )
-}
map :: ( a -> b ) -> Parser a -> Parser b
map f parser input =
    case parser input of
        Fail -> Fail
        Parsed partial ->
            Parsed
                ( Partial
                    { value = f partial.value
                    , backlog = partial.backlog
                    }
                )
