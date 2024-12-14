(** Type variables. *)
type tvar = string

(** Term variables. *)
type var = string

(** Types. *)
(* Question 1.1  *)
(* Simple type *)
type ty = 
  | TVar of tvar
  | Imp of ty * ty
  | And of ty * ty
  | True 
  | Or of ty * ty
  | False
  | Nat

(* Question 1.2 *)

(* lambda-terms *)
type tm = 
  | Var of var
  | App of tm * tm
  | Abs of string * ty * tm
  | Pair of tm * tm
  | Fst of tm
  | Snd of tm 
  | Unit
  | Left of tm * ty
  | Right of ty * tm
  | Case of tm * var * tm * var * tm
  | Absurd of tm * ty
  | Zero 
  | Succ of tm
  | Rec of tm * tm * tm