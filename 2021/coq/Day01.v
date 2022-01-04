(***********)
(* IMPORTS *)
(***********)
From Coq Require Extraction.

Require Import Coq.Arith.PeanoNat.

Require Import Parsec.Core.

Print Visibility.

Definition istchar (a : ascii) : bool :=
  isalpha a ||| isdigit a ||| in_string "!#$%&'*+-.^_`|~" a.

Definition parseToken : parser string :=
  string_of_list_ascii <$> many1 (satisfy istchar).

Goal parse parseToken "GET / HTTP/1.1" = inr ("GET", " / HTTP/1.1").
Proof. reflexivity. Qed.

(*
Definition parse_input : string -> list nat :=
*)

(********************************)
(* Extraction Language: Haskell *)
(********************************)
Extraction Language Haskell.

(***************************)
(* Use Haskell basic types *)
(***************************)
Require Import ExtrHaskellBasic.

Require Import Coq.extraction.ExtrHaskellString.

(****************************************)
(* Use Haskell support for Nat handling *)
(****************************************)
(*Require Import ExtrHaskellNatNum.*)
(* Extract Inductive Datatypes.nat => "Prelude.Integer" ["0" "succ"]
"(\fO fS n -> if n Prelude.== 0 then fO () else fS (n Prelude.- 1))". *)

(***************************)
(* Extract to Haskell file *)
(***************************)
Extraction "./Day01Generated.hs" parseToken parse (* helper helper' *).