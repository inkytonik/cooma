From Coq Require Import Arith.Arith.
From Coq Require Import Bool.Bool.
From Coq Require Import FSets.FMapAVL.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.
From Coq Require Import Structures.OrderedTypeEx.

(* Type Definitions *)
Inductive base : Type :=
  | Bool : base
  | Int : base
  | Str : base
  | Unit : base.

Inductive tipe : Type :=
  | Fun : tipe -> tipe -> tipe
  | Rec : row -> tipe
  | Var : row -> tipe
  | Sec : base -> tipe
  | Pub : base -> tipe
with row : Type :=
  | Row : list (string * tipe) -> row.

(* Expression Definitions *)
Inductive exp : Type :=
  | bool : exp
  | int : exp
  | str : exp
  | unit : exp
  | idn : string -> exp
  | func : string -> tipe -> exp -> exp
  | app : exp -> exp -> exp
  | rec : list (string * exp) -> exp
  | sel : exp -> string -> exp
  | var : string -> exp -> exp
  | mat : exp -> list (string * string * exp) -> exp.

(* Computing security levels *)
Fixpoint sec (t : tipe) : nat :=
  match t with
  | Fun t0 t1 => sec t1
  | Rec (Row r) => 0
  | Var (Row r) => 0
  | Sec _ => 1
  | Pub _ => 0
  end.

Fixpoint sec_prop (t : tipe) : Prop :=
  match t with
  | Fun t0 t1 => (sec t0 <= sec t1) /\ sec_prop t1
  | _ => True
  end.

Example sec_pub_is_0 : sec (Pub Int) = 0.
Proof. reflexivity. Qed.
Example sec_sec_is_1 : sec (Sec Int) = 1.
Proof. reflexivity. Qed.
Example sec_fun_is_t1_1 : sec (Fun (Pub Int) (Sec Int)) = 1.
Proof. reflexivity. Qed.
Example sec_fun_is_t1_2 : sec (Fun (Sec Int) (Pub Int)) = 0.
Proof. reflexivity. Qed.
Example sec_fun_is_t1_3 : sec (Fun (Pub Int) (Fun (Pub Int) (Sec Int))) = 1.
Proof. reflexivity. Qed.

(* Implementing context using Coq maps *)
Module M := FMapAVL.Make(String_as_OT).
Definition context := M.t tipe.
Definition empty := M.empty tipe.
Definition find (gamma: context) x := M.find x gamma.
Definition update (gamma: context) x t := M.add x t gamma.

(* Subtyping rules *)
Inductive subtype : tipe -> tipe -> Prop :=
  | S_Ref : forall (t : tipe),
      subtype t t
  | S_Rec : forall (rt ru : list (string * tipe)),
      incl ru rt ->
      subtype (Rec (Row rt)) (Rec (Row ru))
  | S_Var : forall (rt ru : list (string * tipe)),
      incl rt ru ->
      subtype (Var (Row rt)) (Var (Row ru))
  | S_Fun : forall (t0 t1 u0 u1 : tipe),
      subtype t1 t0 ->
      subtype u0 u1 ->
      subtype (Fun t0 u0) (Fun t1 u1)
  | S_Sec : forall (b : base),
      subtype (Pub b) (Sec b).

(* Subtyping rule tests *)
Example int_sub_int :
    subtype (Pub Int) (Pub Int).
Proof. apply S_Ref. Qed.

Example int_not_bool :
    ~ subtype (Pub Int) (Pub Bool).
Proof. unfold not. intros. inversion H. Qed.

Example pub_int_as_sec : 
    subtype (Pub Int) (Sec Int).
Proof. apply S_Sec. Qed.

Example sec_not_as_pub :
    ~ subtype (Sec Int) (Pub Int).
Proof. unfold not. intros. inversion H. Qed.

Example rec_subtype : forall (a b : string) (t u : tipe),
    subtype (Rec (Row ((a, t)::(b, u)::nil))) (Rec (Row ((a, t)::nil))).
Proof.
  intros. apply S_Rec. apply incl_cons.
  - simpl. left. reflexivity.
  - left. inversion H.  
Qed.

Example var_subtype : forall (a b : string) (t u : tipe),
    subtype (Var (Row ((a, t)::nil))) (Var (Row ((a, t)::(b, u)::nil))).
Proof.
  intros. apply S_Var. apply incl_cons.
  - simpl. left. reflexivity.
  - left. inversion H.
Qed.

(* Typing Rules *)
Inductive type_of : context -> exp -> tipe -> Prop :=
  | T_Bool : forall Gamma,
      type_of Gamma bool (Pub Bool)
  | T_Int : forall Gamma,
      type_of Gamma int (Pub Int)
  | T_Str : forall Gamma,
      type_of Gamma str (Pub Str)
  | T_Unit : forall Gamma,
      type_of Gamma unit (Pub Unit)
  | T_Idn : forall Gamma (x : string) (t : tipe),
      find Gamma x = Some t ->
      sec_prop t -> (* this is our our well-formed context axiom *)
      type_of Gamma (idn x) t
  | T_Fun : forall Gamma (x : string) (e0 : exp) (t0 t1 : tipe),
      type_of (update Gamma x t0) e0 t1 ->
      sec_prop (Fun t0 t1) ->
      type_of Gamma (func x t0 e0) (Fun t0 t1)
  | T_App : forall Gamma (e0 e1 : exp) (t0 t1 : tipe),
      type_of Gamma e0 (Fun t0 t1) ->
      type_check Gamma e1 t0 ->
      type_of Gamma (app e0 e1) t1
  | T_Rec : forall Gamma (rt : list (string * tipe)) (re : list (string * exp)),
      Coq.Init.Datatypes.length rt > 0 -> (* enforce row has at least ONE field per lang. spec. *)
      Forall2 (type_of Gamma) (map snd re) (map snd rt) ->
      Forall sec_prop (map snd rt) ->  
      type_of Gamma (rec re) (Rec (Row rt))
  | T_Var : forall Gamma (e : exp) (t : tipe) (x : string),
      type_of Gamma e t ->
      type_of Gamma (var x e) (Var (Row ((x, t)::nil)))
  | T_Sel : forall Gamma (e : exp) (r : list (string * tipe)) (xi : string) (ti : tipe),
      type_of Gamma e (Rec (Row r)) ->
      sec_prop ti -> (* this is our our well-formed row axiom *)
      type_of Gamma (sel e xi) ti
  | T_Mat : forall Gamma (e : exp) (rv : list (string * tipe)) (cs : list (string * string * exp)) (t0 : tipe),
      Coq.Init.Datatypes.length rv > 0 -> (* enforce row has at least ONE field per lang. spec. *)
      type_of Gamma e (Var (Row rv)) ->
      Forall (fun y => In y (map fst rv)) (map fst (map fst cs)) ->
      Forall2 (fun ci ti => type_of (update Gamma (snd (fst ci)) ti) (snd ci) t0) cs (map snd rv) ->
      Forall (fun ty : tipe => sec_prop (Fun ty t0)) (map snd rv) ->
      type_of Gamma (mat e cs) t0
with type_check : context -> exp -> tipe -> Prop :=
  | T_Wid : forall Gamma (e : exp) (t u : tipe),
      type_of Gamma e u ->
      subtype u t ->
      type_check Gamma e t.

(* Typing rule tests *)
Example bool_is_Pub_Bool : forall Gamma,
    type_of Gamma bool (Pub Bool).
Proof. apply T_Bool. Qed.

Example int_is_Pub_Int : forall Gamma,
    type_of Gamma int (Pub Int).
Proof. apply T_Int. Qed.

Example str_is_Pub_Str : forall Gamma,
    type_of Gamma str (Pub Str).
Proof. apply T_Str. Qed.

Example unit_is_Pub_Unit : forall Gamma,
    type_of Gamma unit (Pub Unit).
Proof. apply T_Unit. Qed. 

Example idn_is_provided_tipe : forall Gamma (x : string),
    type_of (update Gamma x (Pub Int)) (idn x) (Pub Int).
Proof.
  intros. apply T_Idn.
  - unfold find. unfold update. apply M.find_1, M.add_1. reflexivity.
  - unfold sec_prop. reflexivity.
Qed.  

Example func_has_type_t0_t1 : forall (x : string), type_of empty (func x (Pub Int) int) (Fun (Pub Int) (Pub Int)).
Proof.
  intros. apply T_Fun.
  - apply T_Int.
  - unfold sec_prop. split.
    + reflexivity.
    + reflexivity.
Qed.

Example func_sec_to_pub : forall (x : string), ~ type_of empty (func x (Sec Int) int) (Fun (Sec Int) (Pub Int)).
Proof.
  intros. unfold not. intros H. inversion H. inversion H6. inversion H7.
Qed.

Example func_returns_unsafe_fun : forall (x y : string),
  ~ type_of empty (func x (Pub Int) (func y (Sec Int) int)) (Fun (Pub Int) (Fun (Sec Int) (Pub Int))).
Proof.
  intros. unfold not. intros H. inversion H. inversion H3. inversion H13. inversion H14.
Qed.

Example app_has_fun_ret_type : forall Gamma (x : string),
  type_of Gamma (app (func x (Pub Int) (idn x)) int) (Pub Int).
Proof.
  intros. apply T_App with (t0 := (Pub Int)).
  - apply T_Fun.
    + apply T_Idn.
      * unfold find. unfold update. apply M.find_1, M.add_1. reflexivity.
      * reflexivity.
    + unfold sec_prop. split.
      * reflexivity.
      * reflexivity.
  - apply T_Wid with (u := (Pub Int)).
    + apply T_Int.
    + apply S_Ref.  
  - reflexivity. 
Qed.        

Example record_is_Rec : forall Gamma (a b : string),
  type_of Gamma (rec ((a, int)::(b, int)::nil)) (Rec (Row ((a, (Pub Int))::(b, (Pub Int))::nil))).
Proof.
  intros. apply T_Rec. simpl. apply Forall2_cons.
  - apply T_Int.
  - apply Forall2_cons.
    + apply T_Int.
    + apply Forall2_nil.   
Qed.

Example var_is_Var : forall Gamma (a : string),
      type_of Gamma (var a int) (Var (Row ((a, (Pub Int))::nil))).
Proof. intros. apply T_Var. apply T_Int. Qed.

Example rec_field_has_same_type : forall Gamma (a : string),
      type_of Gamma (sel (rec ((a, int)::nil)) a) (Pub Int).
Proof.
  intros. apply T_Sel with (r := (a, (Pub Int))::nil).
  - apply T_Rec. apply Forall2_cons.
    + simpl. apply T_Int.
    + apply Forall2_nil.
  - unfold sec_prop. reflexivity. 
Qed.

(* Ze Expression Security Theroem *)
Theorem expression_security : forall (e : exp) (Gamma : context) (t u : tipe),
  type_of Gamma e (Fun t u) -> 
  sec_prop (Fun t u).
Proof.
  intros e.
  induction e as [| | | | idn | x t0 e0 | e0 IHe0 e1 IHe1 | | | | r x ].
  - (* e = bool *)
    intros. inversion H.
  - (* e = int *)
    intros. inversion H.
  - (* e = str *)
    intros. inversion H.
  - (* e = unit *)
    intros. inversion H.
  - (* e = idn *) 
    intros. inversion H. assumption. 
  - (* e = func *)
    intros. inversion H. inversion H7. assumption.
  - (* e = app *)
    intros. inversion H. subst. apply IHe0 in H3. inversion H3. assumption.   
  - (* e = rec *)
    intros. inversion H.
  - (* e = field sel *)
    intros. inversion H. assumption.
  - (* e = var *) 
    intros. inversion H.
  - (* e = mat *)
    intros. inversion H. inversion H8.
    + assert (Coq.Init.Datatypes.length (map snd rv) = Coq.Init.Datatypes.length rv).
      { apply map_length. }
      rewrite <- H9 in H2. rewrite <- H10 in H2. inversion H2.
    + inversion H10. assumption.
Qed. 
    
    