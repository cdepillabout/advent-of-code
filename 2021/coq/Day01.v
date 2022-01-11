(***********)
(* IMPORTS *)
(***********)
From Coq Require Extraction.

Require Import Coq.Arith.PeanoNat.
Require Import Coq.Numbers.DecimalString.

Require Import Parsec.Core.

Require Import ExtLib.Data.Option.

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

Local Open Scope N_scope.

Fixpoint count_increases (l : list N): N :=
  match l with
  | h1 :: t =>
      match t with
      | h2 :: t2 =>
          (if 1 <=? N.sub h2 h1 then 1 else 0) + count_increases t
      | [] => 0
      end
  | _ => 0
  end.

Example count_increases_example :
  count_increases [199%N; 200%N; 208%N; 210%N; 200%N; 207%N; 240%N; 269%N; 260%N; 263%N] = 7.
Proof. reflexivity. Qed.

Theorem count_increases_adds_one_for_increase :
  forall x y rest, x < y -> 1 + count_increases (y :: rest) = count_increases (x :: y :: rest).
Proof.
  intros x y rest H.
  unfold count_increases at 2.
  set (G := N.leb_spec0 1%N (y - x)).
  inversion G.
  - reflexivity.
  - fold count_increases.
    clear G H1.
    exfalso.
    symmetry in H0.
    apply N.leb_gt in H0.
    apply N.lt_1_r in H0.
    apply N.sub_gt in H.
    apply H. assumption.
  Qed.
  
Theorem lt_is_minus_zero : forall (a b : N), a <= b -> a - b = 0.
Proof.
  induction a.
  - simpl. reflexivity.
  - induction p.
    + simpl. Admitted.
  
Theorem count_increases_same_for_same :
  forall x y rest, y <= x -> count_increases (y :: rest) = count_increases (x :: y :: rest).
Proof.
  intros x y rest H.
  unfold count_increases at 2.
  set (G := N.leb_spec0 1%N (y - x)).
  inversion G.
  - fold count_increases.
    clear G H0.
    exfalso.
    apply N.sub_0_le in H.
    rewrite H in H1.
    apply (N.nle_succ_0 0).
    simpl. assumption.
  - reflexivity.
  Qed.

Existing Instance Functor_option.

Definition solve_part_1 (s : string): option N := count_increases <$> parseInput s.

Fixpoint group_3 (l : list N) : list N := 
  match l with
  | h1 :: t1 =>
      match t1 with
      | h2 :: t2 => 
          match t2 with
          | h3 :: t3 => h1 + h2 + h3 :: group_3 t1
          | [] => []
          end
      | [] => []
      end
  | [] => []
  end.
  
Definition count_increases_in_groups (l : list N) : N := count_increases (group_3 l).
  
Example count_increases_in_groups_example :
  count_increases_in_groups [199%N; 200%N; 208%N; 210%N; 200%N; 207%N; 240%N; 269%N; 260%N; 263%N] = 5.
Proof. reflexivity. Qed.

Definition solve_part_2 (s : string): option N := count_increases_in_groups <$> parseInput s.

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