module Main ( main ) where

    
import Ulme

import qualified Test.QuickCheck    as QuickCheck
import qualified Ulme.List          as List
import qualified Ulme.Parse         as Parse
import qualified Ulme.String        as String

import Test.Hspec               ( describe , hspec , it , parallel , shouldBe )
import Test.Hspec.QuickCheck    ( modifyMaxSuccess )
import Ulme.Parse               ( Parser )

import Test.QuickCheck.Instances.Natural ()


main :: IO ()
main = hspec ( parallel tests )


tests = modifyMaxSuccess ( always 20000 ) <| do

    describe "Parse.string" <| do
    ------------------------------------------------------------------

        it "parses any matching string." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ input ->

            List.all
                ( \ n ->
                    let
                        match = String.left n input
                        tail = String.dropLeft n input
                    in
                        Parse.string match input == Ok ( consume match (0,0) , [ match ] , tail )
                )
                ( List.range 0 ( String.length input )
                )

        it "refuses to parse any non-matching string." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( match , input ) ->

            String.startsWith match input
            || isEqual ( Parse.string match input ) ( Err [ ( (0,0) , errMsg match ) ] )


        it "handles newlines correctly." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ strings ->

            let
                lines = ( strings :: List String ) |> map ( String.filter ( /= '\n' ) ) 
                input = String.join "\n" lines 
                position = case List.head ( List.reverse lines ) of
                    Just last_line -> ( List.length lines -! 1 , String.length last_line )
                    Nothing -> (0,0)
            in
                Parse.string input input == Ok ( position , [ input ] , "" )


        it "passes some more random tests." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ match input ->

            let
                tail = String.dropLeft ( String.length match ) input
            in
                if String.startsWith match input
                then Parse.string match input == Ok ( consume match (0,0) , [ match ] , tail )
                else Parse.string match input == Err [ ( (0,0) , errMsg match ) ]


    describe "Parse.optional" <| do
    ------------------------------------------------------------------

        it "really is optional." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( match , input ) ->

            let
                tail = String.dropLeft ( String.length match )  input
            in
                isEqual
                    ( Parse.optional ( Parse.string match ) input
                    )
                    ( if String.startsWith match input
                      then Ok ( consume match (0,0) , [ match ] , tail )
                      else Ok ( (0,0) , [] , input )
                    )


    describe "Parse.throwAway" <| do
    ------------------------------------------------------------------

        it "throws away the match." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ input ->

            List.all
                ( \ n ->
                    let
                        match = String.left n input
                        tail  = String.dropLeft n input
                        empty = [] :: List String
                    in
                        isEqual 
                            ( Parse.throwAway ( Parse.string match ) input )
                            ( Ok ( consume match (0,0) , empty , tail ) )
                )
                ( List.range 0 ( String.length input )
                )


        it "is NOT optional." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( match , input ) ->

            let
                throwAway string =
                    Parse.throwAway ( Parse.string string ) :: Parser ( List String )
            in
                String.startsWith match input
                || isEqual ( throwAway match input ) ( Err [ ( (0,0) , errMsg match ) ] )


    describe "Parse.zeroOrMore" <| do
    ------------------------------------------------------------------

        it "parses any number of times." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( match , tail , count ) ->

            let
                repeat = String.repeat count match
                result = List.repeat count match
            in
                match == "" -- prevent infinite loop, see (*)
                || String.startsWith match tail
                || isEqual
                    ( Parse.zeroOrMore ( Parse.string match ) ( repeat ++ tail ) )
                    ( Ok ( consume repeat (0,0) , result , tail ) )


        it "is optional." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( match , input ) ->

            match == "" -- prevent infinite loop, see (*)
            || case Parse.zeroOrMore ( Parse.string match ) input of
                Err _ -> False
                Ok _ -> True


    describe "Parse.oneOrMore" <| do
    ------------------------------------------------------------------

        it "parses any positive number of times." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( match , tail , n ) ->

            let
                count  = n + 1
                repeat = String.repeat count match
                result = List.repeat count match
            in
                match == "" -- prevent infinite loop, see (*)
                || String.startsWith match tail
                || isEqual
                    ( Parse.oneOrMore
                        ( Parse.string match )
                        ( String.repeat count match ++ tail )
                    )
                    ( Ok ( consume repeat (0,0) , result , tail )
                    )
            {-
                (*) Greedily parsing any number of empty
                    strings will of course result in an infinite loop.  I could catch that
                    case in `oneOrMore` (recognizing `Parse.string ""` by applying it) and
                    handle it separately, but I'm not sure about the performance penalty this
                    would introduce.
            -}


        it "is NOT optional." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( match , input ) ->

            String.startsWith match input
            || isEqual
                    ( Parse.oneOrMore ( Parse.string match ) input )
                    ( Err [ ( (0,0) , errMsg match ) ] )


    describe "Parse.oneOf" <| do
    ------------------------------------------------------------------

        it "applies the first successful parser." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( left , right , match ,tail ) ->

            let
                input = match ++ tail

                -- remove preceding matches
                filter string = not ( String.startsWith string input )
                filtered_left = List.filter filter left

                strings = filtered_left ++ [ match ] ++ right
                parsers = map Parse.string strings
            in
                isEqual
                    ( Parse.oneOf parsers input )
                    ( Ok ( consume match (0,0) , [ match ] , tail ) )


        it "is NOT optional." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( strings , input ) ->

            let
                filter string = not ( String.startsWith string input )
                clean_strings = List.filter filter strings 
                parsers       = map Parse.string clean_strings
            in
                Parse.oneOf parsers input |> \ case
                    Ok _ -> False
                    Err _ -> True


    describe "Parse.sequence" <| do
    ------------------------------------------------------------------

        it "applies all parsers." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( strings , tail ) ->

            let
                parsers = map Parse.string strings
                input   = ( String.join "" strings ) ++ tail
                allStrs = String.concat strings
            in
                isEqual
                    ( Parse.sequence parsers input )
                    ( Ok ( consume allStrs (0,0) , strings , tail ) )


        it "fails if one parser fails." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ ( left ,right , failing ) ->

            let
                clean str   = not ( String.startsWith str failing )
                clean_left  = List.filter clean left
                clean_right = List.filter clean right
                input       = String.join "" ( clean_left ++ clean_right )

                parsers = map Parse.string ( clean_left ++ [ failing ] ++ clean_right )
            in
                if failing == "" then True -- `Parse.string ""` cannot fail.
                else case Parse.sequence parsers input of
                    Ok _ -> False
                    Err _ -> True


    describe "Parse.map" <| do
    ------------------------------------------------------------------

        it "applies a function to parsing results." <|
        --------------------------------------------------------------
            QuickCheck.property

            \ input ->

            let
                even = map String.length >> List.foldr (+) 0 >> modBy 2
            in
                isEqual
                    ( Parse.map even ( Parse.string input ) input )
                    ( Ok ( consume input (0,0) , even [ input ] , "" ) )



-- Helpers


type Position
    = ( Natural , Natural )


errMsg :: String -> String
errMsg expected =
    "Expecting `" ++ expected ++ "`"


isEqual :: Eq a => a -> a -> Bool
isEqual = (==)


consume :: String -> Position -> Position
{-
    Calculate how many lines and columns we move forward
    when parsing the given input string.
-}
consume input ( line , column ) =
    case input of
    "" -> ( line , column )
    head : tail ->
        if head == '\n'
        then consume tail ( line + 1 , 0 )
        else consume tail ( line , column + 1 )
