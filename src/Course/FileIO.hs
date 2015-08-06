{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main = (run . headOr Nil) =<< getArgs

{-
main = getArgs >>= \args ->
  case args of
    file :. Nil -> run file
    _           -> putStrLn "Usage: runhaskell FileIO.hs filename"
-}

type FilePath = Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run :: Chars -> IO ()
run = printFiles <=< getFiles . lines <=< readFile

{-
run file =
  do
    content <- readFile file
    results <- getFiles (lines content)
    printFiles results
-}

getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles names = sequence $ getFile <$> names

-- getFiles = sequence . (<$>) getFile

getFile :: FilePath -> IO (FilePath, Chars)
getFile name = (,) name <$> readFile name

-- getFile = lift2 (<$>) (,) readFile

printFiles :: List (FilePath, Chars) -> IO ()
printFiles items = void $ sequence $ uncurry printFile <$> items

-- printFiles = void . sequence . (<$>) (uncurry printFile)

printFile :: FilePath -> Chars -> IO ()
printFile name content = putStrLn $ listh "============ " ++ name ++ listh "\n" ++ content

{-
printFile name content =
  putStrLn ("============ " ++ name) >>
  putStrLn content
-}
