module Day01 where

import qualified Prelude

helper' :: Prelude.Integer -> Prelude.Integer -> Prelude.Integer ->
           Prelude.Bool
helper' p m n =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.False)
    (\m' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.False)
      (\_ ->
      (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
        (\_ -> Prelude.False)
        (\n' ->
        (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
          (\_ -> Prelude.False)
          (\_ ->
          (Prelude.||) ((Prelude.==) ((Prelude.*) m n) p) (helper' p m' n))
          n')
        n)
      m')
    m

helper :: Prelude.Integer -> Prelude.Integer -> Prelude.Bool
helper p m =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.False)
    (\m' ->
    (Prelude.||) ((Prelude.==) ((Prelude.*) m m) p)
      ((Prelude.||) (helper' p m' m) (helper p m')))
    m

isPrime :: Prelude.Integer -> Prelude.Bool
isPrime p =
  (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
    (\_ -> Prelude.False)
    (\p' ->
    (\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))
      (\_ -> Prelude.False)
      (\_ -> Prelude.not (helper p p'))
      p')
    p

