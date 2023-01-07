(***********)
(* IMPORTS *)
(***********)
From Coq Require Extraction.

Require Import Coq.Arith.PeanoNat.
Require Import Coq.Numbers.DecimalString.
Require Import Coq.Sorting.Mergesort.
Require Import Coq.Sorting.Sorting.
Require Import Coq.Structures.Orders.
Require Import Coq.micromega.Lia.

Require Import Parsec.Core.

Require Import ExtLib.Data.Option.

Inductive RPS : Type :=
  | Rock : RPS
  | Paper : RPS
  | Scissors : RPS
  .

Definition rpsParser : parser RPS :=
  rawChar <- anyToken ;;
  match rawChar with
  | "A"%char | "X"%char => pure Rock
  | "B"%char | "Y"%char => pure Paper
  | "C"%char | "Z"%char => pure Scissors
  | _ => raise (Some "Not an RPS.")
  end.

Example rpsParserExample1 : parse rpsParser "A" = inr (Rock, ""). Proof. auto. Qed.
Example rpsParserExample2 : parse rpsParser "X" = inr (Rock, ""). Proof. auto. Qed.
Example rpsParserExample3 : parse rpsParser "B" = inr (Paper, ""). Proof. auto. Qed.
Example rpsParserExample4 : parse rpsParser "Y" = inr (Paper, ""). Proof. auto. Qed.
Example rpsParserExample5 : parse rpsParser "C" = inr (Scissors, ""). Proof. auto. Qed.
Example rpsParserExample6 : parse rpsParser "Z" = inr (Scissors, ""). Proof. auto. Qed.

Definition rpsLineParser : parser (RPS * RPS) :=
  oppRps <- rpsParser ;;
  parseSP ;;
  meRps <- rpsParser ;;
  parseLF ;;
  pure (oppRps, meRps).

Example rpsLineParserExample : parse rpsLineParser "A Z
" = inr ((Rock, Scissors), "").
Proof. auto. Qed.

Definition rpsLinesParser : parser (list (RPS * RPS)) := many1 rpsLineParser.

Definition parseInput (str : string): option (list (RPS * RPS)) :=
  match parse rpsLinesParser str with
  | inr (res, "") => Some res
  | _ => None
  end.

Example parseInputExample : parseInput "A Y
B X
C Z
" = Some [(Rock,Paper);(Paper,Rock);(Scissors,Scissors)].
Proof. auto. Qed.

Local Open Scope N_scope.  

Definition myShapeScore (rps : RPS): N :=
  match rps with
  | Rock => 1
  | Paper => 2
  | Scissors => 3
  end.

Definition resultScore (oppRps : RPS) (meRps : RPS): N :=
  match meRps, oppRps with
  | Rock, Scissors | Scissors, Paper | Paper, Rock => 6
  | Rock, Rock | Scissors, Scissors | Paper, Paper => 3
  | _, _ => 0
  end.

Definition compute_round_score (p : RPS * RPS): N :=
  let (oppRps, meRps) := p in
  myShapeScore meRps + resultScore oppRps meRps.

Definition sumNs (l : list N): N := fold_right Nplus N0 l.

Definition compute_total_score (l : list (RPS * RPS)): N := sumNs (map compute_round_score l).

Definition solve_part_1 (s : string): option N := compute_total_score <$> parseInput s.

Example solve_part_1_example : solve_part_1 "A Y
B X
C Z
" = Some 15.
Proof. auto. Qed.

(*
Definition n_to_string (n : N): string :=
  NilEmpty.string_of_uint (N.to_uint n).

Local Open Scope N_scope.  

Definition sum_elf (l : list N): N := fold_right Nplus N0 l.

Example count_increases_example :
  sum_elf [7000%N; 8000%N; 9000%N] = 24000%N.
Proof. reflexivity. Qed.

Definition find_most_elf (elves : list (list N)): N :=
  fold_right N.max 0 (map sum_elf elves).

Theorem find_most_elf_finds_max : forall l n, find_most_elf l = n -> In n (map sum_elf l) \/ (l = [] /\ n = 0).
Proof.
  intro l. induction l.
  - cbv. intros. auto.
  - simpl. unfold find_most_elf in *. simpl in *.
    intros.
    destruct (N.max_dec (sum_elf a) (fold_right N.max 0 (map sum_elf l))).
    * subst. left. left. auto.
    * assert ((sum_elf a) <= (fold_right N.max 0 (map sum_elf l))) by lia.
      rewrite H in e. symmetry in e. specialize (IHl n e).
      inversion IHl.
      + auto.
      + inversion H1. left. left. rewrite H3. rewrite H3 in e. clear H1. lia.
Qed.

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
Extraction "./Day02Generated.hs" parseInput solve_part_1 (* solve_part_2 *).
