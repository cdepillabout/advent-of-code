(***********)
(* IMPORTS *)
(***********)
From Coq Require Extraction.

Require Import Coq.Arith.PeanoNat.

(************)
(* helper'' *)
(************)
Fixpoint helper' (p m n : nat) : bool :=
  match m,n with
  | 0,_ => false
  | 1,_ => false
  | _,0 => false
  | _,1 => false
  | S m',S n' => (orb ((mult m n) =? p) (helper' p m' n))
  end.

(**********)
(* helper *)
(**********)
Fixpoint helper (p m : nat) : bool :=
  match m with
  | 0 => false
  | S m' => (orb ((mult m m) =? p) (orb (helper' p m' m) (helper p m')))
  end.

(***********)
(* isPrime *)
(***********)
Definition isPrime (p : nat) : bool :=
  match p with
  | 0 => false
  | 1 => false
  | S p' => (negb (helper p p'))
  end.

(* Compute (isPrime 220). *)

(********************************)
(* Extraction Language: Haskell *)
(********************************)
Extraction Language Haskell.

(***************************)
(* Use Haskell basic types *)
(***************************)
Require Import ExtrHaskellBasic.

(****************************************)
(* Use Haskell support for Nat handling *)
(****************************************)
Require Import ExtrHaskellNatNum.
Extract Inductive Datatypes.nat => "Prelude.Integer" ["0" "succ"]
"(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))".

(***************************)
(* Extract to Haskell file *)
(***************************)
Extraction "./Day01.hs" isPrime helper helper'.