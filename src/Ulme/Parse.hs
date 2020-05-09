module Ulme.Parse

{-
    A simple parser-combinator module.

    This work was part of my first attempt at non-trivial
    parsing.  It was motivated by this presentation:
    https://vimeo.com/171704565

    Parser-combinators are surprisingly accessible.

    First, we collect tokens as lists of strings using the
    `string` function and some basic parsers.  From these
    tokens we can construct other data using those same
    combinators and the `map` function, perhaps discarding
    tokens using the `throwAway` function.

    For practical reasons most functions in this module
    rely on lists.  We can, of course, parse into any
    data type, but once we leave the (monadic) context of
    a list, we lose the ability to assemble values from
    multiple tokens and to pull apart or throw away values.


    ----

    Copyright 2019-2020, Aramis Concepcion Duran
    
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

( Parser
, string
, optional
, throwAway
, zeroOrMore
, oneOrMore
, oneOf
, sequence
, map
, withError
, errMsg -- exported for testing
)
where


import Ulme hiding ( map )

import qualified Ulme.List      as List
import qualified Ulme.String    as String
import qualified Ulme.Tuple     as Tuple


type Parser a
{-
    A `Parser a` attempts to turn a `String` into a
    value of type `a`.

    If it fails, it results in an `Err errs` where `errs`
    is a list of `ParseError` values.  If it succeeds,
    it results in an `Ok ( n , v , s )` where `n` is the
    number of consumed characters, `v` is a value of type
    `a` and `s` is the remaining input.
-}
    = String -> Result ( List ParseError ) ( Integer , a , String )


type ParseError
{-
    A parse error type.
-}
    = ( Integer , String )


string :: String -> Parser ( List String )
{-
    Parse a `String`.
-}
string match input =
    if String.startsWith match input
    then
        Ok
            ( String.length match
            , [ match ]
            , String.dropLeft ( String.length match ) input
            )
    else
        Err [ ( 0 , "Expecting `" ++ match ++ "`" ) ]


optional :: Parser ( List a ) -> Parser ( List a )
{-
    Optionally apply a parser.
-}
optional parse input =
    case parse input of
    Ok value -> Ok value
    Err _error -> Ok ( 0 , [] , input )


throwAway :: Parser ( List a ) -> Parser ( List b )
{-
    Apply a parser and throw away the result.
-}
throwAway =
    map ( always [] ) >> \ parse input -> case parse input of
    Err errs -> Err errs
    Ok ( n , _done , pending ) -> Ok ( n , [] , pending )


succeed :: a -> Parser a
{-
    Don't consume any input, just succeed unconditionally.

    Useful as a `fold` kick-starter for the sequential
    application of parsers.
-}
succeed value input =
    Ok ( 0 , value , input )


fail :: Parser a
{-
    Don't consume any input, just fail unconditionally.

    Useful as a `fold` kick-starter for trying out parsers
    in parallel until one succeeds.
-}
fail _input =
    Err []


zeroOrMore :: Parser ( List a ) -> Parser ( List a )
{-
    Apply a parser as often as possible.
-}
zeroOrMore parse =
    optional ( oneOrMore parse )


oneOrMore :: Parser ( List a ) -> Parser ( List a )
{-
    Apply a parser as often as possible but at least once.
-}
oneOrMore parse =
    sequence [ parse , optional ( oneOrMore parse ) ]


either :: Parser a -> Parser a -> Parser a
{-
    Apply the first succeeding parser from two
    alternatives.
-}
either parse1 parse2 input =
    case parse1 input of
    Ok value -> Ok value
    Err errs1 ->
        case parse2 input of
        Err errs2 -> Err ( errs1 ++ errs2 )
        Ok value -> Ok value


oneOf :: List ( Parser a ) -> Parser a
{-
    Apply the first succeeding parser from a list of
    parsers.
-}
oneOf parsers =
    List.foldr either fail parsers


succ :: Parser ( List a ) -> Parser ( List a ) -> Parser ( List a )
{-
    Apply two parsers one after the other.
-}
succ parser1 parser2 input =
    case parser1 input of
    Err errs -> Err errs
    Ok ( n1 , done1 , pending1 ) ->
        case parser2 pending1 of
        Err errs ->
            Err ( List.map ( Tuple.mapFirst ( + n1 ) ) errs )
        Ok ( n2 , done2 , pending2 ) ->
            Ok ( n1 + n2 , done1 ++ done2 , pending2 )


sequence :: List ( Parser ( List a ) ) -> Parser ( List a )
{-
    Apply a list of parsers, one after another.
-}
sequence parsers =
    List.foldr succ ( succeed [] ) parsers


map :: ( a -> b ) -> Parser a -> Parser b
{-
    Map over the result of a successful parser.
-}
map fn parse input =
    case parse input of
    Err errs -> Err errs
    Ok ( n , done , pending ) -> Ok ( n , fn done , pending )


withError :: String -> Parser a -> Parser a
{-
    Equip a parser with a custom error message.
-}
withError error parse input =
    case parse input of
    Ok value -> Ok value
    Err errs ->
        case List.head errs of
        Nothing -> Err [ ( 0 , error ) ]
        Just ( n , _ ) -> Err [ ( n , error ) ]



-- Helpers


errMsg :: String -> String -> String
{-
    Print a helpful error message.
-}
errMsg expected _got =
    "Expecting " ++ expected
