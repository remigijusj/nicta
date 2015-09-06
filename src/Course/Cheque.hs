{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Bind

-- $setup
-- >>> :set -XOverloadedStrings

-- A data type representing the digits zero to nine.

data Digit =
    Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded, Show)

-- A data type representing one, two or three digits, which may be useful for grouping.

data Digit3 =
    D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- reversed order!
groupBy3 :: List Digit -> List Digit3
groupBy3 ds = case ds of
  Nil                    -> Nil
  (d1 :. Nil)            -> (D1 d1) :. Nil
  (d1 :. d2 :. Nil)      -> (D2 d2 d1) :. Nil
  (d1 :. d2 :. d3 :. dt) -> (D3 d3 d2 d1) :. (groupBy3 dt)


-- Possibly convert a character to a digit.

fromChar :: Char -> Optional Digit
fromChar c = case c of
  '0' -> Full Zero
  '1' -> Full One
  '2' -> Full Two
  '3' -> Full Three
  '4' -> Full Four
  '5' -> Full Five
  '6' -> Full Six
  '7' -> Full Seven
  '8' -> Full Eight
  '9' -> Full Nine
  _   -> Empty

showDigit :: Digit -> Chars
showDigit d = case d of 
  Zero  -> "zero"
  One   -> "one"
  Two   -> "two"
  Three -> "three"
  Four  -> "four"
  Five  -> "five"
  Six   -> "six"
  Seven -> "seven"
  Eight -> "eight"
  Nine  -> "nine"

showXteen :: Digit -> Chars
showXteen d = case d of
  Zero  -> "ten"
  One   -> "eleven"
  Two   -> "twelve"
  Three -> "thirteen"
  Four  -> "fourteen"
  Five  -> "fifteen"
  Six   -> "sixteen"
  Seven -> "seventeen"
  Eight -> "eighteen"
  Nine  -> "nineteen"

showXty :: Digit -> Chars
showXty d = case d of
  Zero  -> ""
  One   -> "ten"
  Two   -> "twenty"
  Three -> "thirty"
  Four  -> "forty"
  Five  -> "fifty"
  Six   -> "sixty"
  Seven -> "seventy"
  Eight -> "eighty"
  Nine  -> "ninety"

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]

illion :: List Chars
illion =
  let preillion :: List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion :: List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
      baseillion :: List Chars
      baseillion =
        listh [
          ""
        , "thousand"
        , "million"
        , "billion"
        , "trillion"
        , "quadrillion"
        , "quintillion"
        , "sextillion"
        , "septillion"
        , "octillion"
        , "nonillion"
        , "decillion"
        , "undecillion"
        , "duodecillion"
        , "tredecillion"
        , "quattuordecillion"
        , "quindecillion"
        , "sexdecillion"
        , "septendecillion"
        , "octodecillion"
        , "novemdecillion"
        ]
  in baseillion ++ lift2 ((++) =<<) preillion postillion

-- SIMILAR: [x ++ y | x <- preillion, y <- postillion]


-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"


descTriplet :: Digit3 -> Chars
descTriplet t = case t of
  D1 d1 -> showDigit d1
  D2 Zero d1 -> descTriplet (D1 d1)
  D2 One  d1 -> showXteen d1
  D2 d2 Zero -> showXty d2
  D2 d2   d1 -> (showXty d2) ++ "-" ++ (showDigit d1)
  D3 Zero d2 d1   -> descTriplet (D2 d2 d1)
  D3 d3 Zero Zero -> (showDigit d3) ++ " hundred"
  D3 d3 d2 d1     -> (showDigit d3) ++ " hundred and " ++ (descTriplet (D2 d2 d1))


-- TODO: refactor, test
descChunks :: List Digit3 -> List Chars
descChunks (D1 Zero :. Nil) = "zero" :. Nil
descChunks cs =
  let combine = \p s -> if p == "zero" then "" else (if s == "" then p else p ++ " " ++ s)
   in filter (/= "") $ zipWith combine (map descTriplet cs) illion


describe :: Chars -> Chars
describe = unwords . reverse . descChunks . groupBy3 . reverse . listOptional fromChar


describeWith :: Chars -> Chars -> Chars
describeWith unit s = 
  let num = dropWhile (== '0') s
      numz = if num == "" then "0" else num
      suffix = if numz == "1" then "" else "s"
   in describe numz ++ " " ++ unit ++ suffix


dollars :: Chars -> Chars
dollars s =
  let (integer, fraction) = join (***) (filter isDigit) (break (== '.') s)
      decimal = take 2 (fraction ++ "0")
      ds = describeWith "dollar" integer
      cs = describeWith "cent" decimal
   in ds ++ " and " ++ cs
