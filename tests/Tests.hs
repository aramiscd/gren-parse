import Ulme
import qualified Ulme.List          as List
import qualified Ulme.Parse         as Parse
import qualified Ulme.String        as String
import qualified Test.QuickCheck    as QuickCheck

import Ulme.Parse               ( Parser )
import Test.Hspec.QuickCheck    ( modifyMaxSuccess )

import Test.Hspec
    ( describe
 -- , focus
    , hspec
    , it
    , parallel
 -- , pending
 -- , shouldBe
    )


main :: IO ()
main =
    hspec ( parallel tests )


tests = modifyMaxSuccess ( always 10000 ) <| do

    describe "Parse.string" <| do
    ------------------------------------------------------------------

        it "parses any matching string." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ string ->
            let
                input = string :: String
            in
                List.all
                    ( \ n ->
                        let
                            match  = String.left n input
                            tail   = String.dropLeft n input
                            length = String.length match
                        in
                            Parse.string match input
                            == Ok ( length , [ match ] , tail )
                    )
                    ( List.range 0 ( String.length input )
                    )

        it "refuses to parse any non-matching string." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( s1 , s2 ) ->
            let
                match = s1 :: String
                input = s2 :: String
            in
                String.startsWith match input
                || isEqual
                    ( Parse.string match input )
                    ( Err [ ( 0 , Parse.errMsg ( "`" ++ match ++ "`" ) input ) ] )


    describe "Parse.optional" <| do
    ------------------------------------------------------------------

        it "really is optional." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( s1 , s2 ) ->
            let
                match  = s1 :: String
                input  = s2 :: String
                length = String.length match
            in
                isEqual
                    ( Parse.optional ( Parse.string match ) input
                    )
                    ( if String.startsWith match input
                      then Ok ( 0 , [ match ] , String.dropLeft length input )
                      else Ok ( 0 , [] , input )
                    )


    describe "Parse.throwAway" <| do
    ------------------------------------------------------------------

        it "throws away the match." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ input ->
            List.all
                ( \ n ->
                    let
                        match  = String.left n input
                        tail   = String.dropLeft n input
                        length = String.length match
                    in
                        isEqual 
                            ( Parse.throwAway ( Parse.string match ) input )
                            ( Ok ( length , [] :: List String , tail ) )
                )
                ( List.range 0 ( String.length input )
                )

        it "is NOT optional." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( s1 , s2 ) ->
            let
                match = s1 :: String
                input = s2 :: String
            in
                String.startsWith match input
                || isEqual
                    ( ( Parse.throwAway ( Parse.string match ) :: Parser String ) input )
                    ( Err [ ( 0 , Parse.errMsg ( "`" ++ match ++ "`" ) input ) ] )


    describe "Parse.zeroOrMore" <| do
    ------------------------------------------------------------------

        it "parses any number of times." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( s1 , s2 , n ) ->
            let
                match  = s1    :: String
                tail   = s2    :: String
                count  = abs n :: Integer
                length = String.length match * count               
            in
                match == "" -- prevent infinite loop, see (*)
                || isEqual
                    ( Parse.zeroOrMore
                        ( Parse.string match )
                        ( String.repeat count match ++ tail )
                    )
                    ( Ok ( length , List.repeat count match , tail )
                    )

        it "is optional." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( s1 , s2 ) ->
            let
                match = s1 :: String
                input = s2 :: String
            in
                match == "" -- prevent infinite loop, see (*)
                || case Parse.zeroOrMore ( Parse.string match ) input of
                    Ok _ -> True
                    Err _ -> False


    describe "Parse.oneOrMore" <| do
    ------------------------------------------------------------------

        it "parses any positive number of times." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( s1 , s2 , n ) ->
            let
                match  = s1        :: String
                tail   = s2        :: String
                count  = abs n + 1 :: Integer
                length = String.length match * count
            in
                match == "" -- prevent infinite loop, see (*)
                || isEqual
                    ( Parse.oneOrMore
                        ( Parse.string match )
                        ( String.repeat count match ++ tail )
                    )
                    ( Ok ( length , List.repeat count match , tail )
                    )
            {-
                (*) Greedily parsing any number of empty
                    strings will of course result in an
                    infinite loop.  I could catch that
                    case in `oneOrMore` (recognizing
                    `Parse.string ""` by applying it)
                    and handle it separately, but I'm not
                    sure about the performance penalty
                    this would introduce.
            -}

        it "is NOT optional." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( s1 , s2 ) ->
            let
                match = s1 :: String
                input = s2 :: String
            in
                String.startsWith match input
                || isEqual
                    ( Parse.oneOrMore ( Parse.string match ) input )
                    ( Err [ ( 0 , Parse.errMsg ( "`" ++ match ++ "`" ) input ) ] )


    describe "Parse.oneOf" <| do
    ------------------------------------------------------------------

        it "applies the first successful parser." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( l , r , m , t ) ->
            let
                left  = l :: List String
                right = r :: List String
                match = m :: String
                tail  = t :: String

                input = match ++ tail

                -- remove preceding matches
                filter string = not ( String.startsWith string input )
                filtered_left = List.filter filter left

                strings = filtered_left ++ [ match ] ++ right
                parsers = map Parse.string strings
            in
                isEqual
                    ( Parse.oneOf parsers input )
                    ( Ok ( String.length match , [ match ] , tail ) )

        it "is NOT optional." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( ss , i ) ->
            let
                strings = ss :: List String
                input   = i  :: String

                filter string = not ( String.startsWith string input )
                clean_strings = List.filter filter strings 
                parsers       = map Parse.string clean_strings
            in
                Parse.oneOf parsers input |> \ case
                    Err _ -> True
                    Ok _ -> False


    describe "Parse.sequence" <| do
    ------------------------------------------------------------------

        it "applies all parsers." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( ss , t ) ->
            let
                strings = ss :: List String
                tail    = t  :: String

                parsers = map Parse.string ss
                input   = ( String.join "" strings ) ++ tail
                length  = List.foldr (+) 0 ( map String.length strings )
            in
                isEqual
                    ( Parse.sequence parsers input )
                    ( Ok ( length , strings , tail ) )

        it "fails if one parser fails." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ ( l , r , f ) ->
            let
                left    = l :: List String
                right   = r :: List String
                failing = f :: String

                clean str   = not ( String.startsWith str failing )
                clean_left  = List.filter clean left
                clean_right = List.filter clean right
                input       = String.join "" ( clean_left ++ clean_right )

                parsers =
                    map Parse.string
                        ( clean_left ++ [ failing ] ++ clean_right )
            in
                if f == "" then True -- `Parse.string ""` cannot fail.
                else case Parse.sequence parsers input of
                    Err _ -> True
                    Ok _ -> False


    describe "Parse.map" <| do
    ------------------------------------------------------------------

        it "applies a function to parsing results." <|
        --------------------------------------------------------------

            QuickCheck.property <| \ i ->
            let
                input  = i :: String
                even   = map String.length >> List.foldr (+) 0 >> modBy 2
                length = String.length input
            in
                isEqual
                    ( Parse.map even ( Parse.string input ) input )
                    ( Ok ( length , even [ input ] , "" ) )



-- Helpers


isEqual :: Eq a => a -> a -> Bool
isEqual = (==)
