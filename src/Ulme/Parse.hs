module Ulme.Parse

{-
    A simple parser-combinator module.

    This work was part of my first attempt at non-trivial
    parsing.  It was motivated by this presentation:
    https://vimeo.com/171704565

    Parser-combinators are surprisingly accessible.


    ----

    Copyright 2019-2021, Aramis Concepcion Duran

    This file is part of ulme-parse.

    Ulme-parse is free software: you can redistribute it
    and/or modify it under the terms of the GNU General
    Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at
    your option) any later version.

    Ulme-parse is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the
    implied warranty of MERCHANTABILITY or FITNESS FOR
    A PARTICULAR PURPOSE.  See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General
    Public License along with Foobar.  If not, see
    <https://www.gnu.org/licenses/>.
-}

( parse
, string
, fail
, succeed
, eitherOr
, oneOf
, andThen
, skip
, optional
, sequence
, oneOrMore
, zeroOrMore
, map
)
where

import Ulme hiding ( andThen , map , sequence )

import Data.Monoid ( mempty )
import Ulme.List qualified as List
import Ulme.String qualified as String


type Parser a = String -> Parsed a
data Parsed a = Fail | Parsed ( Partial a ) deriving Show
data Partial a = Partial { value :: a , backlog :: String } deriving Show


parse :: Parser a -> String -> Maybe a
parse parser input =
    case parser input of
        Fail -> Nothing
        Parsed partial ->
            if backlog partial /= ""
            then Nothing
            else Just ( value partial )


string :: String -> String -> Parsed String
string match input =
    case String.uncons match of
        Nothing ->
            Parsed
                ( Partial
                    { value = match
                    , backlog = input
                    }
                )
        Just ( pHead , pTail ) ->
            case String.uncons input of
                Nothing -> Fail
                Just ( sHead , sTail ) ->
                    if pHead /= sHead
                    then Fail
                    else
                        case string pTail sTail of
                            Fail -> Fail
                            Parsed partial ->
                                Parsed
                                    ( Partial
                                        { value = String.cons pHead ( value partial )
                                        , backlog = backlog partial
                                        }
                                    )


fail :: Parser a
fail input = Fail


succeed :: a -> Parser a
succeed value input =
    Parsed ( Partial { value = value , backlog = input } )


eitherOr :: Parser a -> Parser a -> Parser a
eitherOr parserA parserB input =
    case parserA input of
        Fail -> parserB input
        Parsed partial -> Parsed partial


oneOf :: List ( Parser a ) -> Parser a
oneOf parsers =
    List.foldl eitherOr fail parsers


andThen :: Semigroup a => Parser a -> Parser a -> Parser a
andThen parserB parserA input =
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


skip :: Monoid a => Parser a -> Parser a
skip parser input =
    case parser input of
        Fail -> Fail
        Parsed partial ->
           Parsed
                ( Partial
                    { value = mempty
                    , backlog = backlog partial
                    }
                )


optional :: Monoid a => Parser a -> Parser a
optional parser input =
    case parser input of
        Parsed partial -> Parsed partial
        Fail ->
            Parsed
                ( Partial
                    { value = mempty
                    , backlog = input
                    }
                )


sequence :: Monoid a => List ( Parser a ) -> Parser a
sequence parsers =
    List.foldl andThen ( succeed mempty ) parsers


oneOrMore :: Monoid a => Parser a -> Parser a
oneOrMore parser =
    sequence [ parser , optional ( oneOrMore parser ) ]


zeroOrMore :: Monoid a => Parser a -> Parser a
zeroOrMore parser =
    optional ( oneOrMore parser )


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
