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

Definition computeMyRps (oppRps : RPS) (neededResult : RPS): RPS :=
  match oppRps with
  | Rock =>
      match neededResult with
      | Rock => Scissors (* need to lose *)
      | Paper => Rock (* need to draw *)
      | Scissors => Paper (* need to win *)
      end
  | Paper =>
      match neededResult with
      | Rock => Rock (* need to lose *)
      | Paper => Paper (* need to draw *)
      | Scissors => Scissors (* need to win *)
      end
  | Scissors =>
      match neededResult with
      | Rock => Paper (* need to lose *)
      | Paper => Scissors (* need to draw *)
      | Scissors => Rock (* need to win *)
      end
  end.

Definition play_round (p : RPS * RPS): N :=
  let (oppRps, neededResult) := p in
  let meRps := computeMyRps oppRps neededResult in
  myShapeScore meRps + resultScore oppRps meRps.

Definition compute_total_score_part_2 (l : list (RPS * RPS)): N := sumNs (map play_round l).

Definition solve_part_2 (s : string): option N := compute_total_score_part_2 <$> parseInput s.

Example solve_part_2_example : solve_part_2 "A Y
B X
C Z
" = Some 12.
Proof. auto. Qed.


Definition n_to_string (n : N): string :=
  NilEmpty.string_of_uint (N.to_uint n).


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
Extraction "./Day02Generated.hs" parseInput n_to_string solve_part_1 solve_part_2.
