(***********)
(* IMPORTS *)
(***********)
From Coq Require Extraction.

Require Import Coq.Arith.PeanoNat.
Require Import Coq.Numbers.DecimalString.
Require Import Coq.Sorting.Mergesort.
Require Import Coq.Sorting.Sorting.
Require Import Coq.Structures.Orders.

Require Import Parsec.Core.

Require Import ExtLib.Data.Option.

Definition singleNumParser : parser N :=
  n <- parseDec ;;
  parseLF ;;
  pure n.

Print singleNumParser.

Example singleNumParserExample : parse singleNumParser "123
" = inr (123%N, "").
Proof. unfold parse. simpl. reflexivity. Qed.

Definition elfNumsParser : parser (list N) := many1 singleNumParser.

Example elfNumsParserExample : parse elfNumsParser "199
200
208
210
200
" = inr ([199%N; 200%N; 208%N; 210%N; 200%N], "").
Proof. unfold parse. simpl. reflexivity. Qed.



Definition many_' {T : Type} (acc : list T) (fuel : nat) (p : @parser string T) :
    @parser string (list T) :=
  match fuel with
  | O => pure []
  | S fuel' => pure []
  end.

Definition sepBy1 {P} {T} {SEP} (p : parser T)
    (s : @parser P SEP) : @parser P (list T) :=
  x <- p ;;
  xs <- many (s ;; p);;
  pure (x :: xs). 

Definition elvesParser : parser (list (list N)) :=
  sepBy1 elfNumsParser parseLF.

Example elvesParserExample : parse elvesParser "199
200
208

10000

5000
6000
" = inr ([[199%N; 200%N; 208%N]; [10000%N]; [5000%N; 6000%N]], "").
Proof. auto. Qed.


Definition parseInput (str : string): option (list (list N)) :=
  match parse elvesParser str with
  | inr (res, "") => Some res
  | _ => None
  end.
  
Example parseInputExample : parseInput "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
" = Some [[1000%N;2000%N;3000%N];[4000%N];[5000%N; 6000%N];[7000%N; 8000%N; 9000%N];[10000%N]].
Proof. auto. Qed.

Definition n_to_string (n : N): string :=
  NilEmpty.string_of_uint (N.to_uint n).

Local Open Scope N_scope.  

Definition sum_elf (l : list N): N := fold_right Nplus N0 l.

Example count_increases_example :
  sum_elf [7000%N; 8000%N; 9000%N] = 24000%N.
Proof. reflexivity. Qed.

Definition find_most_elf (elves : list (list N)): N :=
  fold_right N.max 0 (map sum_elf elves).

Definition solve_part_1 (s : string): option N := find_most_elf <$> parseInput s.

Module NOrder <: TotalLeBool.
  Definition t := N.
  Definition leb x y := N.leb x y.
  Print leb.
  (* Infix "<=?" := leb (at level 70, no associativity). *)
  Theorem leb_total : forall a1 a2, leb a1 a2 = true \/ leb a2 a1 = true.
  Proof.
    intros a1 a2. 
    set (G := N.leb_spec a1 a2). 
    inversion G.
    - auto.
    - set (J := N.leb_spec a2 a1).
      inversion J.
      * right. auto.
      * exfalso. set (K := N.lt_asymm a1 a2 H2). auto.
  Qed.
End NOrder.

Module Import NSort := Sort NOrder.

Definition sort_elves (elves : list (list N)): list N :=
  sort (map sum_elf elves).

Definition solve_part_2 (s : string): option (list N) := sort_elves <$> parseInput s.

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
Extraction "./Day01Generated.hs" parseInput n_to_string solve_part_1 solve_part_2 (* helper helper' *).
