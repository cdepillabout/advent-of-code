{-# LANGUAGE TemplateHaskell #-}

module Main where

import TemplateLib (createInput)


type InputDay01 = $(createInput "./day01-input-example.txt")

main :: IO ()
main = pure ()
