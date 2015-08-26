{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Interactive where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.Traversable
import Course.List
import Course.Optional

-- | Eliminates any value over which a functor is defined.

vooid :: Functor m => m a -> m ()
vooid = (<$>) (const ())

-- void x = () <$ x


-- | A version of @bind@ that ignores the result of the effect.
-- equivalent to Monad (>>) or Applicative (*>)

(>-) :: Monad m => m a -> m b -> m b
(>-) a = (>>=) a . const

-- a >- b = a >>= \_ -> b


-- | Runs an action until a result of that action satisfies a given predicate.
-- p = The predicate to satisfy to stop running the action.
-- a = The action to run until the predicate satisfies.
-- vooid . untilM equivalent to (Control.Monad.Loops) untilM_

untilM :: Monad m => (a -> m Bool) -> m a -> m a
untilM p a =
  a   >>= \r ->
  p r >>= \q ->
  if q then pure r
       else untilM p a


-- helper for usage elow

checkQuit :: Char -> IO Bool
checkQuit c =
  if c == 'q'
    then
      putStrLn "Bye!" >-
      pure True
    else
      pure False


-- | Example program that uses IO to echo back characters that are entered by the user.

echo :: IO ()
echo =
  vooid $ untilM checkQuit (
    putStr "Enter a character: " >-
    getChar >>= \c ->
    putStrLn "" >-
    putStrLn (c :. Nil) >-
    pure c)


-- |
--
-- * Ask the user to enter a string to convert to upper-case.
--
-- * Convert the string to upper-case.
--
-- * Print the upper-cased string to standard output.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @toUpper :: Char -> Char@ -- (Data.Char) converts a character to upper-case.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.

convertInteractive :: IO ()
convertInteractive =
  putStr "Enter a string: " >-
  getLine >>= \s ->
  let u = map toUpper s in
  putStrLn u


-- |
--
-- * Ask the user to enter a file name to reverse.
--
-- * Ask the user to enter a file name to write the reversed file to.
--
-- * Read the contents of the input file.
--
-- * Reverse the contents of the input file.
--
-- * Write the reversed contents to the output file.
--
-- /Tip:/ @getLine :: IO String@ -- an IO action that reads a string from standard input.
--
-- /Tip:/ @readFile :: FilePath -> IO String@ -- an IO action that reads contents of a file.
--
-- /Tip:/ @writeFile :: FilePath -> String -> IO ()@ -- writes a string to a file.
--
-- /Tip:/ @reverse :: [a] -> [a]@ -- reverses a list.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.

reverseInteractive :: IO ()
reverseInteractive =
  putStr "Enter a file to read: " >-
  getLine >>= \fi ->
  putStr "Enter a file to write: " >-
  getLine >>= \fo ->
  readFile fi >>= \s ->
  writeFile fo (reverse s)


-- |
--
-- * Ask the user to enter a string to url-encode.
--
-- * Convert the string with a URL encoder.
--
-- * For simplicity, encoding is defined as:
--
-- * @' ' -> \"%20\"@
--
-- * @'\t' -> \"%09\"@
--
-- * @'\"' -> \"%22\"@
--
-- * @/anything else is unchanged/@
--
-- * Print the encoded URL to standard output.
--
-- /Tip:/ @putStr :: String -> IO ()@ -- Prints a string to standard output.
--
-- /Tip:/ @putStrLn :: String -> IO ()@ -- Prints a string and then a new line to standard output.

encodeChar :: Char -> Chars
encodeChar ' '  = "%20"
encodeChar '\t' = "%09"
encodeChar '\"' = "%22"
encodeChar c = pure c

-- Note: could use List (>>=) as flatMap here

encodeInteractive :: IO ()
encodeInteractive =
  putStr "Enter a string: " >-
  getLine >>= \s ->
  let e = flatMap encodeChar s in
  putStrLn e


data Op = Op Char Chars (IO ()) -- keyboard entry, description, program


interactive :: IO ()
interactive =
  let ops = (
               Op 'c' "Convert a string to upper-case" convertInteractive
            :. Op 'r' "Reverse a file" reverseInteractive
            :. Op 'e' "Encode a URL" encodeInteractive
            :. Op 'q' "Quit" (pure ())
            :. Nil
            )
  in vooid (untilM checkQuit
             (putStrLn "Select: " >-
              traverse (\(Op c s _) ->
                putStr (c :. Nil) >-
                putStr ". " >-
                putStrLn s) ops >-
              getChar >>= \c ->
              putStrLn "" >-
              let o = find (\(Op c' _ _) -> c' == c) ops
                  r = case o of
                        Empty -> (putStrLn "Not a valid selection. Try again." >-)
                        Full (Op _ _ k) -> (k >-)
              in r (pure c)))
