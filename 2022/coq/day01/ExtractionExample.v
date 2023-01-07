(***********)
(* IMPORTS *)
(***********)
From Coq Require Extraction.

Require Import Coq.Arith.PeanoNat.

Definition parse_

(* Extraction Language: Haskell *)
Extraction Language Haskell.


(* Use Haskell basic types *)
Require Import ExtrHaskellBasic.


(* Use Haskell support for Nat handling *)
Require Import ExtrHaskellNatNum.
Extract Inductive Datatypes.nat => "Prelude.Integer" ["0" "succ"]
  "(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".

(***************************)
(* Extract to Haskell file *)
(***************************)
Extraction "./ExtractionExample.hs" isPrime (* helper helper' *).