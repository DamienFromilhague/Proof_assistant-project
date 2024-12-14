let () = Printexc.record_backtrace true

open Expr

let debug = false
let ty_of_string s = Parser.ty Lexer.token (Lexing.from_string s)
let tm_of_string s = Parser.tm Lexer.token (Lexing.from_string s)

(* Question 1.3  *)

let rec string_of_ty (t : ty) : string = 
  match t with
  | TVar a -> a
  | Imp (t1, t2) -> 
    let left = match t1 with
      | Imp (_, _) -> "(" ^ string_of_ty t1 ^ ")"  (* Ajoute des parenthèses pour les sous-types en flèche *)
      | And (_,_) ->  "(" ^ string_of_ty t1 ^ ")" 
      | _ -> string_of_ty t1
    in
    let right = match t2 with
      | Imp (_, _) -> string_of_ty t2  (* Pas besoin pour la droite *)
      | _ -> string_of_ty t2
    in
    left ^ " => " ^ right
  | And (t1, t2) -> 
    let left = match t1 with
     | Imp (_,_) -> "(" ^ string_of_ty t1 ^ ")"
     | _ -> string_of_ty t1
    in  
    let right = match t2 with
     | Imp (_,_) -> "(" ^ string_of_ty t2 ^ ")"
     | _ -> string_of_ty t2
    in
    left ^ " /\\ " ^ right
  | True -> "\u{22a4}"  
  | Or (t1, t2) -> "(" ^ string_of_ty t1 ^ " \\/ " ^ string_of_ty t2 ^ ")"
  | False -> "\u{22a5}"  
  | Nat -> "\u{2115}"  

let rec raw_of_ty ty =
  match ty with
  | TVar v -> v
  | Imp (t1, t2) -> "Imp(" ^ raw_of_ty t1 ^ "," ^ raw_of_ty t2 ^ ")"
  | And (t1, t2) -> "And(" ^ raw_of_ty t1 ^ "," ^ raw_of_ty t2 ^ ")"
  | Or (t1, t2) -> "Or(" ^ raw_of_ty t1 ^ "," ^ raw_of_ty t2 ^ ")"
  | True -> "True"
  | False -> "False"  
  | Nat -> "Nat"  


let rec string_of_tm (t : tm) : string = 
  match t with
  | Var a -> a
  | App (t1, t2) -> "(" ^ string_of_tm t1 ^ " " ^ string_of_tm t2 ^ ")"
  | Abs (x, ty, t_body) ->  
    "(fun (" ^ x ^ " : " ^ string_of_ty ty ^ ") -> " ^ string_of_tm t_body ^ ")"
  | Pair (t1, t2) ->
      "\u{27e8}" ^ string_of_tm t1 ^ "," ^ string_of_tm t2 ^ "\u{27e9}"
  | Fst t1 -> "\u{1D6D1}1(" ^ string_of_tm t1 ^ ")"
  | Snd t1 -> "\u{1D6D1}2(" ^ string_of_tm t1 ^ ")"    
  | Unit -> "\u{27e8}\u{27e9}"
  | Left (tm, b) -> "\u{1d704}l" ^ string_of_ty b ^ "(" ^ string_of_tm tm ^ ")"
  | Right (a, tm) -> "\u{1d704}r" ^ string_of_ty a ^ "(" ^ string_of_tm tm ^ ")"
  | Case (tm, x, u, y, v) ->
    "case(" ^ string_of_tm tm ^ ", " ^ x ^ ", " ^ string_of_tm u ^ ", " ^ y
    ^ ", " ^ string_of_tm v ^ ")"
  | Absurd (t, a) -> "case" ^ string_of_ty a ^ "(" ^ string_of_tm t ^ ")" 
  | Zero -> string_of_int 0
  | Succ n -> "S(" ^ (string_of_tm n) ^ ")"
  | Rec (n, z, s) -> "rec(" ^ (string_of_tm n) ^ ", " ^ (string_of_tm z) ^ ", " ^ (string_of_tm s) ^ ")";;
           



(* Question 1.4 *)

type context = (var * ty) list
exception Type_error

(** On définit la fonction qui infère le type d'un terme, selon le contexte donné c*)
(* let rec infer_type (ctx : context)(t : tm) : ty =
  match t with
  | Var x -> (try List.assoc x ctx
with Not_found -> raise Type_error)
  | Abs (x, ty_x, t_body) ->
    (* On utilise la règle -> I *)
    let ctx' = (x, ty_x) :: ctx in (* On étend le contexte à x*)
    let ty_body = infer_type ctx' t_body in
    Imp (ty_x, ty_body) (* Le type cherché *)
  | App(t1, t2) ->
    let ty1 = infer_type ctx t1 in
    let ty2 = infer_type ctx t2 in
    (match ty1 with
    (* On utilise la règle ->E *)
    | Imp (ty_arg, ty_ret) ->
      if ty_arg = ty2 then ty_ret
      else raise Type_error
    | _ -> raise Type_error) *)


(* Tests question 1.4

(* Valid Term *)
let term_valid =
  Abs ("f", Imp (Tvar "A", Tvar "B"),
    Abs ("g", Imp (Tvar "B", Tvar "C"),
      Abs ("x", Tvar "A",
        App (Var "g", App (Var "f", Var "x"))
      )
    )
  )

let () =
  let inferred_type = infer_type [] term_valid in
  Printf.printf "Valid term type: %s\n" (string_of_ty inferred_type)

(* Invalid Term 1: Free variable *)
let term_invalid_1 = Abs ("f", Tvar "A", Var "x")

let () =
  let result =
    try let _ = infer_type [] term_invalid_1 in false
    with Type_error -> true
  in
  Printf.printf "Invalid term 1 (free variable): %b\n" result

(* Invalid Term 2: Non-function application *)
let term_invalid_2 =
  Abs ("f", Tvar "A",
    Abs ("x", Tvar "B",
      App (Var "f", Var "x")
    )
  )

let () =
  let result =
    try let _ = infer_type [] term_invalid_2 in false
    with Type_error -> true
  in
  Printf.printf "Invalid term 2 (non-function application): %b\n" result

(* Invalid Term 3: Argument type mismatch *)
let term_invalid_3 =
  Abs ("f", Imp (Tvar "A", Tvar "B"),
    Abs ("x", Tvar "B",
      App (Var "f", Var "x")
    )
  )

let () =
  let result =
    try let _ = infer_type [] term_invalid_3 in false
    with Type_error -> true
  in
  Printf.printf "Invalid term 3 (argument type mismatch): %b\n" result *)

(* Question 1.5 *)

(* let check_type (ctx : context) (t : tm) (expected_ty : ty) : unit =
  let inferred_ty = infer_type ctx t in
  if inferred_ty = expected_ty then ()
  else raise Type_error *)

(* 
(* Tests question 1.5 *)
let () = 
  Printf.printf "Starting tests...\n";
  flush stdout;  (* Assurez-vous que la sortie est immédiatement affichée *)

  (* Term 1: λ(x: A). x *)
  let term_identity = Abs ("x", Tvar "A", Var "x") in

  (* Test 1 *)
  Printf.printf "Running Test 1: check if λ(x: A).x has type A → A...\n";
  flush stdout;

  try
    check_type [] term_identity (Imp (Tvar "A", Tvar "A"));
    Printf.printf "Test 1 passed.\n";
    flush stdout;
  with Type_error ->
    Printf.printf "Test 1 failed.\n";
    flush stdout;
    
  (* Test 2 *)
  Printf.printf "Running Test 2: check if λ(x: A).x does not have type B → B...\n";
  flush stdout;
  try
    check_type [] term_identity (Imp (Tvar "B", Tvar "B"));
    Printf.printf "Test 2 failed.\n";
    flush stdout;
  with Type_error ->
    Printf.printf "Test 2 passed.\n";
    flush stdout;

  (* Test 3 *)
  Printf.printf "Running Test 3: check if variable x has type A...\n";
  flush stdout;

  let term_free_var = Var "x" in
  try
    check_type [] term_free_var (Tvar "A");
    Printf.printf "Test 3 failed.\n";
    flush stdout;
  with Type_error ->
    Printf.printf "Test 3 passed.\n";
    flush stdout; *)

(* Question 1.6 *)

(** On définit la fonction qui infère le type d'un terme, selon le contexte donné de manière mutuellement récursive avec check_type*)
let rec infer_type (ctx : context)(t : tm) : ty =
  match t with
  | Var x -> (try List.assoc x ctx
with Not_found -> raise Type_error)
  | Abs (x, ty_x, t_body) ->
    (* On utilise la règle -> I *)
    let ctx' = (x, ty_x) :: ctx in (* On étend le contexte à x*)
    let ty_body = infer_type ctx' t_body in
    Imp (ty_x, ty_body) (* Le type cherché *)
  | App(t1, t2) -> (
    match infer_type ctx t1 with
    | Imp (a, b) ->
      check_type ctx t2 a;
      b
    | _ -> raise Type_error  )
  | Pair (t1, t2) -> And (infer_type ctx t1, infer_type ctx t2)  
  | Fst t1 -> (
      match infer_type ctx t1 with And (t2, _) -> t2 | _ -> raise Type_error)
  | Snd t1 -> (
      match infer_type ctx t1 with And (_, t3) -> t3 | _ -> raise Type_error)
  | Unit -> True    
  | Left (t, b) -> Or (infer_type ctx t, b)
  | Right (a, t) -> Or (a, infer_type ctx t)
  | Case (t, x, u, y, v) -> (
      match infer_type ctx t with
      | Or (a, b) -> (
          match
            (infer_type ((x, a) :: ctx) u, infer_type ((y, b) :: ctx) v)
          with
          | c1, c2 when c1 = c2 -> c1
          | _ -> raise Type_error)
      | _ -> raise Type_error)
  | Absurd (t, a) -> (
     match infer_type ctx t with False -> a | _ -> raise Type_error)     
     (* Question 3.1 *)
  | Zero -> Nat
  | Succ n -> (match infer_type ctx n with
                | Nat -> Nat
                | _ -> raise Type_error)
  | Rec (n, z, s) ->
    (match ((infer_type ctx n), (infer_type ctx z), (infer_type ctx s)) with
      | (Nat, t1, Imp(Imp(Nat, t2), t3))
      | (Nat, t1, Imp(Nat, Imp(t2, t3))) when t1 = t2 && t2 = t3 -> t1
      | _ -> raise Type_error)

and check_type ctx t a = if infer_type ctx t <> a then raise Type_error

(* Question 1.8 *)

(* on définit un terme qui représente la commutativité de la conjonction logique *)
let () =
  let and_comm =
    Abs ("t", And (TVar "A", TVar "B"), Pair (Snd (Var "t"), Fst (Var "t")))
  in
  print_endline (string_of_tm and_comm);
  print_endline (string_of_ty (infer_type [] and_comm))

(* Question 1.9 *)

let () =
  let truth = Abs ("f", Imp (True, TVar "A"), App (Var "f", Unit)) in
  print_endline (string_of_tm truth);
  print_endline (string_of_ty (infer_type [] truth))

(* Question 1.10 *)

let () =
  let or_comm =
    Abs
      ( "t",
        Or (TVar "A", TVar "B"),
        Case
          ( Var "t",
            "x",
            Right (TVar "B", Var "x"),
            "y",
            Left (Var "y", TVar "A") ) )
  in
  print_endline (string_of_tm or_comm);
  print_endline (string_of_ty (infer_type [] or_comm))

(*  Question 1.11 *)

let () =
  let fals =
    Abs
      ( "t",
        And (TVar "A", Imp (TVar "A", False)),
        Absurd (App (Snd (Var "t"), Fst (Var "t")), TVar "B") )
  in
  print_endline (string_of_tm fals);
  print_endline (string_of_ty (infer_type [] fals))

(* Question 1.12 *)

let () =
  let l = [
    "A => B";
    "A ⇒ B";
    "A /\\ B";
    "A ∧ B";
    "T";
    "A \\/ B";
    "A ∨ B";
    "_";
    "not A";
    "¬ A";
  ]
  in
  List.iter
    (fun s ->
       Printf.printf
         "the parsing of %S is %s\n%!" s (string_of_ty (ty_of_string s))
    ) l

let () =
  let l = [
    "t u v";
    "fun (x : A) -> t";
    "λ (x : A) → t";
    "(t , u)";
    "fst(t)";
    "snd(t)";
    "()";
    "case t of x -> u | y -> v";
    "left(t,B)";
    "right(A,t)";
    "absurd(t,A)"
  ]
  in
  List.iter
    (fun s ->
       Printf.printf
         "the parsing of %S is %s\n%!" s (string_of_tm (tm_of_string s))
    ) l  



 

(* Question 2.1 *)

let string_of_ctx (ctx : context) : string =
  String.concat ", " (List.map (fun (x, t) -> x ^ " : " ^ string_of_ty t) ctx)

(* On teste la fonction avec l'exemple donné dans la consigne *)
let () =
  let ctx =
    [
      ("x", Imp (TVar "A", TVar "B"));
      ("y", And (TVar "A", TVar "B"));
      ("Z", TVar "T");
    ]
  in
  print_endline (string_of_ctx ctx)  

(* Question 2.2 *)

type sequent = context * ty

let string_of_seq ((ctx, t) : sequent) : string = 
  string_of_ctx ctx ^ " |- " ^ string_of_ty t

(* On teste avec l'exemple *)

let () =
  let seq =
    ([ ("x", Imp (TVar "A", TVar "B")); ("y", TVar "A") ], TVar "B")
  in
  print_endline (string_of_seq seq)

(* Question 2.3 *)

(* copie de proving.ml *)

let rec prove env a file =
  print_endline (string_of_seq (env,a));
  print_string "? "; flush_all ();
  let error e = print_endline e; prove env a file in
  let cmd, arg =
    let cmd = input_line stdin in
    let n = try String.index cmd ' ' with Not_found -> String.length cmd in
    let c = String.sub cmd 0 n in
    let a = String.sub cmd n (String.length cmd - n) in
    let a = String.trim a in
    c, a
  in
  let () = output_string file (cmd ^ " " ^ arg ^ "\n") in
  match cmd with
  | "intro" ->
     (
       match a with
       | Imp (a, b) ->
          if arg = "" then error "Please provide an argument for intro." else
            let x = arg in
            let t = prove ((x,a)::env) b file in
            Abs (x, a, t)
       (* Question 2.7 *)
       | And (a, b) ->
          let t1 = prove env a file in
          let t2 = prove env b file in
          Pair (t1, t2)
       (* Question 2.8 *)
       | True -> Unit
       (* Question 3.2 *)
       | Nat ->
        if arg = "" then Zero
        else if List.exists (fun (x, _) -> x = arg) env then
          error "This variable is already in the environment."
        else
          let t = prove ((arg, Nat) :: env) Nat file in
          Succ t       
       | _ ->
          error "Don't know how to introduce this."
     )
  | "exact" ->
     let t = tm_of_string arg in
     if infer_type env t <> a then error "Not the right type."
     else t

(* Question 2.4 *)

  | "elim" ->
    let f = tm_of_string arg in
    (match infer_type env f with
    | Imp (f1, f2) when f2 = a ->
      let t = prove env f1 file in
      App (f, t)
    | Imp (_, _) -> error "Not the right type."
    | Or (l, r) -> let t1 = prove ((arg ^ "1", l)::env) a file in
                  let t2 = prove ((arg ^ "2", r)::env) a file in
                  Case (f, arg ^ "1", t1, arg ^ "2", t2)
    (* 2.10 Falsity *)
    | False -> Absurd (f, a)
    | Nat -> (
      if not (String.contains arg '|') then
        error "Please input rank step x and step y in the form \"x | y\""
      else
        match String.split_on_char '|' arg with
        | x :: y :: _ ->
            let t = prove env Nat file in
            let u = prove env a file in
            let v =
              prove
                ((x, Nat) :: (y, a) :: env)
                a file
            in
            Rec (t, u, Abs (x, Nat, Abs (y, a, v)))
        | _ -> error "Not enough arguments provided")    
    | _ -> error "Need to provide an argument to elim.")

(* Question 2.6 *)
  | "cut" ->
    let b = ty_of_string arg in
    let t1 = prove env (Imp (b, a)) file in
    let t2 = prove env b file in
    App (t1, t2)

(* Question 2.7 *)
  | "fst" ->
    let p = tm_of_string arg in
    (match infer_type env p with
    | And (t1, _) -> if t1 <> a then error "Erreur : mauvais type"
                      else (Fst p)
    | _ -> error "Seulement possible pour conjonction")
  | "snd" ->
    let p = tm_of_string arg in
    (match infer_type env p with
    | And (_, t2) -> if t2 <> a then error "Erreur : mauvais type"
                      else (Snd p)
    | _ -> error "Seulement possible pour conjonction")

(* Question 2.8 *)
  | "left" ->
    (match a with
    | Or (a,b) -> let t = prove env a file in
                  Left(t, b)
    | _ -> error "Seulement possible pour disjoction")
  | "right" ->
    (match a with
    | Or (a,b) -> let t = prove env b file in
                  Right  (a, t)
    | _ -> error "Seulement possible pour disjonction")

  | cmd -> error ("Unknown command: " ^ cmd)

let () =
  print_endline "Please enter the formula to prove:";
  let a = input_line stdin in
  let a = ty_of_string a in
  print_endline "Let's prove it.";

  (* Question 2.5 *)
  let file = open_out "temp.proof" in
  let t = prove [] a file in
  print_endline "done.";
  print_endline "Proof term is";
  print_endline (string_of_tm t);
  print_string  "Typechecking... "; flush_all ();
  assert (infer_type [] t = a);
  print_endline "ok."