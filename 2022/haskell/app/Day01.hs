{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude.Singletons -- (Map, Sum)
import Data.List.Singletons

import TemplateLib (createRawInput)

type InputDay01 = $(createRawInput "./day01-input.txt")

type SolvePart1 input = Last (Sort (Map SumSym0 input))

main :: IO ()
main = pure ()
