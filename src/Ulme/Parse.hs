{- |
    Copyright   : (c) 2019-2022 Aramís Concepción Durán
    License     : GPL-3.0-only
    Maintainer  : Aramís Concepción Durán <aramis@systemli.org>

    A simple parser-combinator module.

    This work was part of my first attempt at non-trivial parsing.  It was motivated by this presentation:
    https://vimeo.com/171704565

    Parser-combinators are surprisingly accessible.
-}
module Ulme.Parse
    ( Parser
    , Parsed ( Fail , Parsed )
    , Partial ( Partial , value , backlog )
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


type Parser a = String -> Parsed a
data Parsed a = Fail | Parsed ( Partial a ) deriving ( Show )
data Partial a = Partial { value :: a , backlog :: String } deriving ( Show )


parse :: Parser a -> String -> Maybe a
parse parser input =
    case parser input of
        Parsed partial -> if backlog partial == "" then Just ( value partial ) else Nothing
        Fail -> Nothing


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
                                    { value = String.cons pHead ( value partial )
                                    , backlog = backlog partial
                                    }
                                )


fail :: Parser a
fail _input = Fail


succeed :: a -> Parser a
succeed value input = Parsed ( Partial { value = value , backlog = input } )


eitherOr :: Parser a -> Parser a -> Parser a
eitherOr parserA parserB input =
    case parserA input of
        Parsed partial -> Parsed partial
        Fail -> parserB input


oneOf :: List ( Parser a ) -> Parser a
oneOf parsers = List.foldr eitherOr fail parsers


skip :: Monoid a => Parser a -> Parser a
skip parser input =
    case parser input of
        Parsed partial -> Parsed ( Partial { value = mempty , backlog = backlog partial } )
        Fail -> Fail


optional :: Monoid a => Parser a -> Parser a
optional parser input =
    case parser input of
        Parsed partial -> Parsed partial
        Fail -> Parsed ( Partial { value = mempty , backlog = input } )


pair :: Semigroup a => Parser a -> Parser a -> Parser a
pair parserA parserB input =
    case parserA input of
        Fail -> Fail
        Parsed partialA ->
            case parserB ( backlog partialA ) of
                Fail -> Fail
                Parsed partialB ->
                    Parsed
                        ( Partial
                            { value = value partialA ++ value partialB
                            , backlog = backlog partialB
                            }
                        )


sequence :: Monoid a => List ( Parser a ) -> Parser a
sequence parsers = List.foldr pair ( succeed mempty ) parsers


oneOrMore :: Monoid a => Parser a -> Parser a
oneOrMore parser = sequence [ parser , optional ( oneOrMore parser ) ]


zeroOrMore :: Monoid a => Parser a -> Parser a
zeroOrMore parser = optional ( oneOrMore parser )


map :: ( a -> b ) -> Parser a -> Parser b
map f parser input =
    case parser input of
        Fail -> Fail
        Parsed partial ->
            Parsed
                ( Partial
                    { value = f ( value partial )
                    , backlog = backlog partial
                    }
                )
