module Main ( main ) where

import Ulme

import Test.Hspec ( describe , hspec , it , parallel {-, shouldBe-} )
import Test.Hspec.Core.Spec ( SpecM )
import Test.Hspec.QuickCheck ( modifyMaxSuccess )
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()
import Ulme.List qualified as List
import Ulme.Parse ( parse )
import Ulme.Parse qualified as Parse
import Ulme.String qualified as String


main :: IO ()
main = tests |> modifyMaxSuccess ( always 9999 ) |> parallel |> hspec


tests :: SpecM () ()
tests = do
    describe "Parse.string" <| do
        it "succeeds on matching strings"
            <| QuickCheck.property \ input -> do
                let parser = Parse.string input
                parse parser input == Just input

        it "fails on non-matching strings"
            <| QuickCheck.property \ match input -> do
                match == input || do
                    let parser = Parse.string match
                    parse parser input == Nothing

    describe "Parse.fail" <| do
        it "fails"
            <| QuickCheck.property \ input -> do
                parse Parse.fail input == Nothing

    describe "Parse.succeed" <| do
        it "succeeds"
            <| QuickCheck.property \ value -> do
                let parser = Parse.succeed value
                parse parser "" == Just value

    describe "Parse.eitherOr" <| do
        it "succeeds if the first parser succeeds"
            <| QuickCheck.property \ input -> do
                let parser = Parse.eitherOr ( Parse.string input ) Parse.fail
                parse parser input == Just input

        it "succeeds if the second parser succeeds"
            <| QuickCheck.property \ input -> do
                let parser = Parse.eitherOr Parse.fail ( Parse.string input )
                parse parser input == Just input

        it "fails if both parsers fail"
            <| QuickCheck.property \ input -> do
                let parser = Parse.eitherOr Parse.fail Parse.fail
                parse parser input == Nothing

    describe "Parse.oneOf" <| do
        it "applies the first successful parser"
            <| QuickCheck.property \ left input right -> do
                let filter = List.filter ( \ s -> not ( String.startsWith s input ) )
                let parsers = map Parse.string ( filter left ++ [ input ] ++ filter right )
                parse ( Parse.oneOf parsers ) input == Just input

        it "fails if no parser succeeds"
            <| QuickCheck.property \ left input right -> do
                let filter = List.filter ( \ s -> not ( String.startsWith s input ) )
                let parsers = map Parse.string ( filter left ++ filter right )
                parse ( Parse.oneOf parsers ) input == Nothing

    describe "Parse.skip" <| do
        it "skips the match"
            <| QuickCheck.property \ match -> do
                let parser = Parse.skip ( Parse.string match )
                parse parser match == Just ""

        it "fails without a match"
            <| QuickCheck.property \ match input -> do
                match == input || do
                    let parser = Parse.skip ( Parse.string match )
                    parse parser input == Nothing

    describe "Parse.optional" <| do
        it "succeeds with a succeeding parser"
            <| QuickCheck.property \ input -> do
                let parser = Parse.optional ( Parse.string input )
                parse parser input == Just input

        it "succeeds with a failing parser"
            <| QuickCheck.property \ match -> do
                match == "" || do
                    let parser = Parse.optional ( Parse.string match )
                    parse parser "" == Just ""

    describe "Parse.sequence" <| do
        it "applies all parsers"
            <| QuickCheck.property \ inputA inputB -> do
                let parserA = Parse.string inputA
                let parserB = Parse.string inputB
                let parserAB = Parse.sequence [ parserA , parserB ]
                parse parserAB ( inputA ++ inputB ) == Just ( inputA ++ inputB )

        it "fails if one parser fails"
            <| QuickCheck.property \ left right -> do
                let input = String.concat ( left ++ right )
                let parseLeft = map Parse.string left
                let parseRight = map Parse.string right
                let parseLFR = List.concat [ parseLeft , List.singleton Parse.fail , parseRight ]
                let parser = Parse.sequence parseLFR
                parse parser input == Nothing

    describe "Parse.oneOrMore" <| do
        it "parses any positive number of times"
            <| QuickCheck.property \ input n -> do
                input == "" || do
                    let inputN = String.repeat ( n + 1 ) input
                    let parser = Parse.oneOrMore ( Parse.string input )
                    parse parser inputN == Just inputN

        it "fails if there is no match"
            <| QuickCheck.property \ match input -> do
                String.startsWith match input || do
                    let parser = Parse.oneOrMore ( Parse.string match )
                    parse parser input == Nothing

    describe "Parse.zeroOrMore" <| do
        it "parses any number of times"
            <| QuickCheck.property \ input n -> do
                input == "" || do
                    let inputN = String.repeat n input
                    let parser = Parse.zeroOrMore ( Parse.string input )
                    parse parser inputN == Just inputN

    describe "Parse.map" <| do
        it "fails on a failing parser"
            <| QuickCheck.property \ input -> do
                let parser = Parse.map identity Parse.fail
                parse parser input == Nothing

        it "succeeds on a successful parser"
            <| QuickCheck.property \ input -> do
                let parser = Parse.map identity ( Parse.string input )
                parse parser input == Just input

        it "applies a function to the parsed value"
            <| QuickCheck.property \ input -> do
                let parser = Parse.map String.length ( Parse.string input )
                parse parser input == Just ( String.length input )
