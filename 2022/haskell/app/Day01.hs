{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoCUSKs #-}
{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.List.Singletons
import Data.Singletons.TH
import GHC.TypeLits.Singletons
import Prelude.Singletons -- (Map, Sum)

import TemplateLib (createRawInput)
import GHC.TypeLits (Nat)


$(promoteOnly [d|
    groupSpan :: [Symbol] -> [[Symbol]]
    groupSpan [] = []
    groupSpan (h : t) =
      case span (/= "") t of
        (xs, []) -> [h : xs]
        (xs, _ : ys) -> (h : xs) : groupSpan ys

    myspan :: (Char -> Bool) -> Symbol -> (Symbol, Symbol)
    myspan p s =
      case unconsSymbol s of
        Nothing -> ("", "")
        Just (x, xs') ->
          if p x then
            let (ys, zs) = myspan p xs'
            in (consSymbol x ys, zs)
          else
            ("", s)

    mybreak :: (Char -> Bool) -> Symbol -> (Symbol, Symbol)
    mybreak p =  myspan (not . p)

    mylines :: Symbol -> [Symbol]
    mylines sym =
      case unconsSymbol sym of
        Nothing -> []
        Just (_, _) ->
          let (l, s') = mybreak (== '\n') sym
          in
          l :
            case unconsSymbol s' of
              Nothing -> []
              Just (_, s'') -> mylines s''

    mygroup :: [Symbol] -> [[Symbol]]
    mygroup = reverse . go [] []
      where
        go :: [[Symbol]] -> [Symbol] -> [Symbol] -> [[Symbol]]
        go totalAccum thisAccum [] = reverse thisAccum : totalAccum
        go totalAccum thisAccum ("" : rest) = go (reverse thisAccum : totalAccum) [] rest
        go totalAccum thisAccum (h : rest) = go totalAccum (h : thisAccum) rest

    readIntAccum :: Natural -> Symbol -> Natural
    readIntAccum accum sym =
      case unconsSymbol sym of
        Nothing -> accum
        Just (h, t) ->
          let newAccum = accum * 10 + charToNat h - charToNat '0'
          in readIntAccum newAccum t
          -- let newAccum = accum * 10 + (charToNat h - charToNat '0')
          -- in readIntAccum newAccum t

    readInt :: Symbol -> Natural
    readInt = readIntAccum 0

  |])

type InputDay01 :: Symbol
type InputDay01 = $(createRawInput "./day01-input-example.txt")

type ParseElves :: Symbol -> [[Natural]]
type ParseElves rawInput = Map (MapSym1 ReadIntSym0) (Mygroup (Mylines rawInput))

type ComputeMost :: [[Natural]] -> Natural
type ComputeMost elves = Last (Sort (Map SumSym0 elves))

type SolvePart1 :: Symbol -> Natural
type SolvePart1 rawInput = ComputeMost (ParseElves rawInput)

solve :: SolvePart1 InputDay01 ~ 100 => IO ()
solve = pure ()

main :: IO ()
main = solve
