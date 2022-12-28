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

module TemplateLib where

import Language.Haskell.TH.Syntax (Q, Type, addDependentFile)
import Language.Haskell.TH (appT, promotedConsT, numTyLit, promotedNilT, litT, runIO, Type(PromotedNilT), strTyLit)
import Data.Foldable (foldrM)

createInput :: FilePath -> Q Type
createInput fp = do
  addDependentFile fp
  rawInput <- runIO $ readFile fp
  let rawInputGroups = group $ lines rawInput
      inputGroups = fmap (fmap (read :: String -> Int)) rawInputGroups
  foldrM f PromotedNilT inputGroups
  where
    f :: [Int] -> Type -> Q Type
    f is rest = appT (appT promotedConsT (foldrM g PromotedNilT is)) (pure rest)

    g :: Int -> Type -> Q Type
    g i rest = appT (appT promotedConsT (litT (numTyLit $ fromIntegral i))) (pure rest)

group :: [String] -> [[String]]
group = reverse . go [] []
  where
    go :: [[String]] -> [String] -> [String] -> [[String]]
    go totalAccum thisAccum [] = reverse thisAccum : totalAccum
    go totalAccum thisAccum ("" : rest) = go (reverse thisAccum : totalAccum) [] rest
    go totalAccum thisAccum (h : rest) = go totalAccum (h : thisAccum) rest

createRawInput :: FilePath -> Q Type
createRawInput fp = do
  addDependentFile fp
  rawInput <- runIO $ readFile fp
  litT $ strTyLit rawInput
