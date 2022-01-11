(***********)
(* IMPORTS *)
(***********)
From Coq Require Extraction.

Require Import Coq.Arith.PeanoNat.
Require Import Coq.Numbers.DecimalString.

Require Import Parsec.Core.

(*
Print Visibility.

Definition istchar (a : ascii) : bool :=
  isalpha a ||| isdigit a ||| in_string "!#$%&'*+-.^_`|~" a.

Definition parseToken : parser string :=
  string_of_list_ascii <$> many1 (satisfy istchar).

Goal parse parseToken "GET / HTTP/1.1" = inr ("GET", " / HTTP/1.1").
Proof. reflexivity. Qed.
*)

Example parseDecExample : parse parseDec "123" = inr (123%N, "").
Proof. unfold parse. simpl. reflexivity. Qed.


Example parseDecExample2 : parse parseDec "123
" = inr (123%N, "
").
Proof. unfold parse. simpl. reflexivity. Qed.

Example parseLFExample : parse parseLF "
" = inr ("010"%char, "").
Proof. unfold parse. simpl. reflexivity. Qed.


Definition singleNumParser : parser N :=
  n <- parseDec ;;
  parseLF ;;
  pure n.

Example singleNumParserExample : parse singleNumParser "123
" = inr (123%N, "").
Proof. unfold parse. simpl. reflexivity. Qed.

Definition numsParser : parser (list N) := many1 singleNumParser.

Example numsParserExample : parse numsParser "199
200
208
210
200
" = inr ([199%N; 200%N; 208%N; 210%N; 200%N], "").
Proof. unfold parse. simpl. reflexivity. Qed.

Definition parseInput (str : string): option (list N) :=
  match parse numsParser str with
  | inr (res, "") => Some res
  | _ => None
  end.
  
Example parseInputExample : parseInput "199
200
208
210
200
" = Some [199%N; 200%N; 208%N; 210%N; 200%N].
Proof. unfold parseInput. unfold parse. simpl. reflexivity. Qed.

Print N.

Print positive.

Compute (xI xH).

Definition n_to_string (n : N): string := NilEmpty.string_of_uint (N.to_uint n).

Compute (n_to_string 100).
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
Extraction "./Day01Generated.hs" parseInput n_to_string (* helper helper' *).