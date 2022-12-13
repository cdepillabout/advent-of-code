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

Inductive count_increases_relation : (list N) -> N -> Prop :=
  | Count_Increases_EmptyList : count_increases_relation [] 0
  | Count_Increases_SingleItem (x : N) : count_increases_relation [x] 0
  | Count_Increases_LT (x y amt : N) (rest : list N) :
      x < y ->
      count_increases_relation (y :: rest) amt ->
      count_increases_relation (x :: y :: rest) (1 + amt)
  | Count_Increases_NLT (x y amt : N) (rest : list N) :
      x >= y ->
      count_increases_relation (y :: rest) amt ->
      count_increases_relation (x :: y :: rest) amt
  .
  
Lemma gt_1_implies_lt : forall (x y : N), 1 <= x - y -> y < x.
Proof.
  (* unfold N.lt. unfold N.le. unfold not.*) (* unfold N.compare. *)
  intros x y.
  destruct (1 ?= x - y) eqn:E.
  + apply N.compare_eq in E.
    symmetry in E.
    assert (1 <> 0).
    { intros H. discriminate H. }
    set (G := N.add_sub_eq_nz x y 1 H E).
    rewrite <- G.
    intros.
    apply N.lt_add_pos_r. reflexivity.
  + intros H.
    clear E.
    Check N.le_add_le_sub_r.
    induction y using N.peano_rect.
    - simpl in H.
      unfold N.lt.
       (* unfold N.le in H.
    (* rewrite <- (N.le_add_le_sub_r 1 x y) in H. *)
    set (G :=N.le_add_le_sub_r 1 x y). *)
    Admitted.
  
Theorem count_increases_relation_true :
  forall l amt, count_increases l = amt <-> count_increases_relation l amt.
Proof.
  split.
  - destruct l as [ | h1 [ | h2 rest ] ].
    + simpl. intros H. subst amt. constructor.
    + simpl. intros H. subst amt. constructor.
    + simpl. 
      set (G := N.leb_spec0 1%N (h2 - h1)).
      inversion G.
      * intros H1.
        subst amt.
        apply Count_Increases_LT.
        symm
        Admitted.
    

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
  
Theorem group_3_is_three :
  forall x y z rest, group_3 (x :: y :: z :: rest) = (x + y + z) :: group_3 (y :: z :: rest).
Proof.
  intros. simpl. reflexivity.
Qed.
  
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