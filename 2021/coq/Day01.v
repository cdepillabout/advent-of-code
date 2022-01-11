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
  
Theorem one_le_n_pos : forall p, 1%N <= N.pos p.
Proof.
  intro p.
  unfold N.le.
  unfold not.
  Print N.compare.
  simpl.
  Admitted.
  (*
  induction p.
  - destruct (1 ?= p~1)%positive eqn:E.
    + admit.
    + admit.
    + 
  - intros. discriminate H.
  - intros.
  Print N.le.
  *)

Example count_increases_example :
  count_increases [199%N; 200%N; 208%N; 210%N; 200%N; 207%N; 240%N; 269%N; 260%N; 263%N] = 7.
Proof. reflexivity. Qed.

Theorem y_minus_x_is_pos : forall (y x : N) (p : positive), y - x = N.pos p -> exists z, y = N.pos z.
Proof.
  induction y.
  - simpl. intros. discriminate H.
  - intros.
  


Theorem count_increases_adds_one_for_increase :
  forall x y rest, x < y -> 1 + count_increases (y :: rest) = count_increases (x :: y :: rest).
Proof.
  (*
  intros x y rest.
  generalize dependent x.
  generalize dependent y.
  induction rest.
  - intros y x HXltY. assert (1 <= y - x).
    { admit. }
    Print reflect.
    Check N.leb_spec0.
    set (G := N.leb_spec0 1%N (y - x)).
    inversion G.
    * simpl. rewrite <- H0. reflexivity.
    * apply H1 in H. destruct H.
  -
  *) 
  (*intros x y.
  generalize dependent x.
  induction y as [|y'].
  - admit.
  - *)
  (*
  intros x y rest H.
  destruct x,y,rest; try (inversion H).
  - simpl.
  *)
  intros x y rest H.
  unfold count_increases at 2.
  set (G := N.leb_spec0 1%N (y - x)).
  inversion G.
  - reflexivity.
  - fold count_increases.
    exfalso. apply H1.
    clear H0 H1 G.
    unfold N.le.
    unfold not.
    intro H1.
    unfold N.compare in H1.
    unfold N.lt in H.
    unfold N.compare in H.
    destruct (y - x) eqn:E.
    + destruct x.
      * rewrite N.sub_0_r in E. rewrite E in H. discriminate H.
      * clear H1.
        destruct y.
        -- discriminate H.
        -- apply N.sub_0_le in E. unfold N.le in E. unfold not in E.
           apply E. apply N.lt_gt. apply H.
    + 
  

Existing Instance Functor_option.

Definition solve (s : string): option N := count_increases <$> parseInput s.

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
Extraction "./Day01Generated.hs" parseInput n_to_string solve (* helper helper' *).