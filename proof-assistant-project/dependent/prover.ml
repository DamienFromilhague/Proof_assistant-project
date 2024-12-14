let () = Printexc.record_backtrace true

open Expr

let of_string s = Parser.expr Lexer.token (Lexing.from_string s)


(* Question 5.2 *)

let rec to_string e =
  match e with
  | Type -> "type"
  | Var x -> x
  | App (u, v) -> "(" ^ to_string u ^ " " ^ to_string v ^ ")"
  | Abs (x, t, u) ->
      "(fun (" ^ x ^ " : " ^ to_string t ^ ") -> " ^ to_string u ^ ")"
  | Pi (x, a, b) -> "((" ^ x ^ " : " ^ to_string a ^ ") -> " ^ to_string b ^ ")"
  | Nat -> "N"
  | Z -> "Z"
  | S n -> "(S " ^ to_string n ^ ")"
  | Ind (p, z, s, n) ->
      "(Ind " ^ to_string p ^ " " ^ to_string z ^ " " ^ to_string s ^ " "
      ^ to_string n ^ ")"
  | Eq (t, u) -> "(" ^ to_string t ^ " = " ^ to_string u ^ ")"
  | Refl t -> "(refl " ^ to_string t ^ ")"
  | J (p, r, x, y, e) ->
      "(J " ^ to_string p ^ " " ^ to_string r ^ " " ^ to_string x ^ " "
      ^ to_string y ^ " " ^ to_string e ^ ")"

(* Question 5.3 *)
let n = ref 0

let fresh_var () =
  let x = "x" ^ string_of_int !n in
  incr n;
  x

(* Question 5.4 *)
let rec subst x t u =
  match u with
  | Type -> Type
  | Var y when y = x -> t
  | Var y -> Var y
  | App (v, w) -> App (subst x t v, subst x t w)
  | Abs (y, ty, v) ->
      let y' = fresh_var () in
      let v' = subst y (Var y') v in
      Abs (y', subst x t ty, subst x t v')
  | Pi (y, a, b) ->
      let y' = fresh_var () in
      let b' = subst y (Var y') b in
      Pi (y', subst x t a, subst x t b')
  | Nat -> Nat
  | Z -> Z
  | S n -> S (subst x t n)
  | Ind (p, z, s, n) -> Ind (subst x t p, subst x t z, subst x t s, subst x t n)
  | Eq (v, w) -> Eq (subst x t v, subst x t w)
  | Refl v -> Refl (subst x t v)
  | J (p, r, y, y', e) ->
      J (subst x t p, subst x t r, subst x t y, subst x t y', subst x t e)

(* Question 5.5 *)
type context = (var * (expr * expr option)) list

let rec string_of_context ctx =
  match ctx with
  | [] -> ""
  | (x, (t, v)) :: l ->
      let st = to_string t in
      let sv = match v with None -> "" | Some v -> " = " ^ to_string v in
      let endline = if List.length l > 0 then "\n" else "" in
      x ^ " : " ^ st ^ sv ^ endline ^ string_of_context l

(* Question 5.6 *)

(* On peut supposer que toutes les fonctions sont bien-typées d'après l'énoncé *)
(* Avant de définir la fonction normalize, je dois définir une fonction de réduction *)

let rec red ctx e =
  match e with
  | Type -> None
  | Var x -> (
      (* Vérifie si une valeur est associée à la variable dans le contexte *)
      match List.assoc_opt x ctx with
      | None -> None
      | Some (_, None) -> None
      | Some (_, Some v) -> Some v)
  | App (Abs (x, _, u), t) -> Some (subst x t u)
  | App (u, v) -> (
      (* Réduction de chaque composant d'une application *)
      match (red ctx u, red ctx v) with
      | None, None -> None
      | Some u', None -> Some (App (u', v))
      | None, Some v' -> Some (App (u, v'))
      | Some u', Some v' -> Some (App (u', v')))
  | Abs (x, tx, v) -> (
      match (red ctx tx, red ((x, (tx, None)) :: ctx) v) with
      | None, None -> None
      | Some tx', None -> Some (Abs (x, tx', v))
      | None, Some v' -> Some (Abs (x, tx, v'))
      | Some tx', Some v' -> Some (Abs (x, tx', v')))
  | Pi (x, a, b) -> (
      (* Réduction des termes d'un produit *)
      match (red ctx a, red ctx b) with
      | None, None -> None
      | Some a', None -> Some (Pi (x, a', b))
      | None, Some b' -> Some (Pi (x, a, b'))
      | Some a', Some b' -> Some (Pi (x, a', b')))
  | Nat -> None
  | Z -> None
  | S n -> ( match red ctx n with None -> None | Some n' -> Some (S n'))
  | Ind (p, z, s, n) -> (
      match red ctx n with
      | None -> (
          (* Cas particuliers pour Ind lorsque n est une constante *)
          match n with
          | Z -> Some z
          | S n' -> Some (App (App (s, n'), Ind (p, z, s, n')))
          | _ -> None)
      | Some n' -> Some n')
  | Eq (t, u) -> (
      (* Réduction des termes d'une égalité *)
      match (red ctx t, red ctx u) with
      | None, None -> None
      | Some t', None -> Some (Eq (t', u))
      | None, Some u' -> Some (Eq (t, u'))
      | Some t', Some u' -> Some (Eq (t', u')))
  | Refl t -> ( match red ctx t with None -> None | Some t' -> Some (Refl t'))
  | J (_, r, x, y, e) when x = y && e = Refl x -> 
      (* Cas spécial de J lorsque x = y et e est une réflexion *)
      Some (App (r, x))
  | J (p, r, x, y, e) -> (
      (* Réduction récursive sur chaque composant de J *)
      match (red ctx p, red ctx r, red ctx x, red ctx y, red ctx e) with
      | None, None, None, None, None -> None
      | Some p', _, _, _, _ -> Some (J (p', r, x, y, e))
      | _, Some r', _, _, _ -> Some (J (p, r', x, y, e))
      | _, _, Some x', _, _ -> Some (J (p, r, x', y, e))
      | _, _, _, Some y', _ -> Some (J (p, r, x, y', e))
      | _, _, _, _, Some e' -> Some (J (p, r, x, y, e')))


(* On peut maintenant définir la fonction normalize à partir de red *)

let rec normalize ctx e =
  match red ctx e with None -> e | Some u -> normalize ctx u

(* Question 5.7 *)
let rec alpha terme1 terme2 =
  match (terme1, terme2) with
  | Type, Type -> true (* Les deux sont du type "Type", donc égaux *)
  | Var var1, Var var2 when var1 = var2 -> true (* Les variables sont identiques *)
  | App (t1, t2), App (t1', t2') -> alpha t1 t1' && alpha t2 t2' (* Application : les deux parties doivent être égales *)
  | Abs (var1, type1, corps1), Abs (var2, type2, corps2) ->
      let nouvelle_var = fresh_var () in
      (* Substitue la variable dans le corps des deux abstractions *)
      let corps1' = subst var1 corps1 (Var nouvelle_var) in
      let corps2' = subst var2 corps2 (Var nouvelle_var) in
      alpha type1 type2 && alpha corps1' corps2'
  | Pi (var1, type1, corps1), Pi (var2, type2, corps2) ->
      let nouvelle_var = fresh_var () in
      (* Substitue la variable dans les corps des deux pi-types *)
      let corps1' = subst var1 corps1 (Var nouvelle_var) in
      let corps2' = subst var2 corps2 (Var nouvelle_var) in
      alpha type1 type2 && alpha corps1' corps2'
  | Nat, Nat -> true (* Les deux termes représentent les nombres naturels *)
  | Z, Z -> true (* Les deux termes représentent zéro *)
  | S n1, S n2 -> alpha n1 n2 (* Vérifie l'égalité pour les successeurs *)
  | Ind (p1, z1, s1, n1), Ind (p2, z2, s2, n2) ->
      (* Induction : les composants doivent tous être égaux *)
      alpha p1 p2 && alpha z1 z2 && alpha s1 s2 && alpha n1 n2
  | Eq (terme1, terme2), Eq (terme1', terme2') ->
      (* Égalité : les deux côtés doivent être égaux *)
      alpha terme1 terme1' && alpha terme2 terme2'
  | Refl terme1, Refl terme2 -> alpha terme1 terme2 (* Réflexivité *)
  | J (p1, r1, x1, y1, e1), J (p2, r2, x2, y2, e2) ->
      (* Égalité dépendante : vérifie chaque composant *)
      alpha p1 p2 && alpha r1 r2 && alpha x1 x2 && alpha y1 y2 && alpha e1 e2
  | _ -> false (* Tous les autres cas ne sont pas égaux *)

(* Question 5.8 *)
(* On utilise les deux dernières fonctions qu'on a défini : normalize et alpha *)
let conv ctx t u =
  let t' = normalize ctx t in
  let u' = normalize ctx u in
  alpha t' u'

(* Question 5.9 *)
exception Type_error of string

let err s = raise (Type_error s)

let rec infer ctx e =
  match e with
  | Type -> Type
  | Var x -> (
      (* Recherche du type associé à la variable x dans le contexte *)
      match List.assoc_opt x ctx with
      | Some (t, _) -> t
      | None -> err ("Unkown type for variable " ^ x))
  | App (u, v) -> (
      (* Inférence des types des deux termes de l'application *)
      let tu = infer ctx u in
      let tv = infer ctx v in
      match tu with
      | Pi (x, tx, w) when conv ctx tx tv -> subst x v w
      | _ ->
          (* Erreur si les types ne correspondent pas pour une application *)
          err
            ("Term of type " ^ to_string tv ^ " is applied to term of type "
           ^ to_string tu))
  | Abs (x, tx, u) ->
      (* Une abstraction est typée par un Pi *)
      Pi (x, tx, infer ((x, (tx, None)) :: ctx) u)
  | Pi (_, _, _) -> Type
  | Nat -> Type
  | Z -> Nat
  | S n ->
      (* Vérification que n est de type Nat *)
      check ctx n Nat;
      Nat
  | Ind (p, z, s, n) ->
      (* Vérifie que n est un naturel *)
      check ctx n Nat;
      let tp = infer ctx p in
      (* Vérifie que p est de type Nat -> Type *)
      if conv ctx tp (Pi ("", Nat, Type)) then
        let tz = infer ctx z in
        let pz = App (p, Z) in
        (* Vérifie que z est de type p Z *)
        if conv ctx tz pz then
          let ts = infer ctx s in
          match ts with
          | Pi (n', c, d) when conv ctx c Nat ->
              (* Vérifie le type de s *)
              if
                conv
                  ((n', (Nat, None)) :: ctx)
                  d
                  (Pi ("", App (p, Var n'), App (p, S (Var n'))))
              then normalize ctx (App (p, n))
              else err ("Wrong type (2) for s : " ^ to_string ts)
          | _ -> err ("Wrong type (1) for s : " ^ to_string ts)
        else
          err
            ("z should be of type " ^ to_string pz ^ " but is of type "
           ^ to_string tz)
      else err ("Invalid type for p which is of type " ^ to_string tp)
  | Eq (_, _) -> Type
  | Refl t -> Eq (t, t)
  | J (p, r, x, y, e) -> (
      (* Vérifie le type de p (première étape) *)
      let tp = infer ctx p in
      match tp with
      | Pi (a, tx, w) -> (
          (match w with
          | Pi (b, tx', z) ->
              (* Vérifie que w est un Pi avec les bons types *)
              if conv ctx tx tx' then
                if
                  conv
                    ((b, (tx', None)) :: (a, (tx, None)) :: ctx)
                    z
                    (Pi ("", Eq (Var a, Var b), Type))
                then () (* Le type de p est correct *)
                else err ("Invalid type for p (4) : " ^ to_string z)
              else
                err ("Invalid type for p (3) : " ^ to_string tx ^ to_string tx')
          | _ -> err ("Invalid type for p (2) : " ^ to_string w));
          (* Vérifie les types de x et y *)
          check ctx x tx;
          check ctx y tx;
          (* Vérifie le type de r *)
          let tr = infer ctx r in
          match tr with
          | Pi (x', tx', ro) ->
              (* Vérifie que tx et tx' correspondent *)
              if conv ctx tx tx' then
                let eval_refl =
                  App (App (App (p, Var x'), Var x'), Refl (Var x'))
                in
                (* Vérifie que ro est bien du type attendu *)
                if conv ((x', (tx, None)) :: ctx) ro eval_refl then
                  (* Vérifie le type de e *)
                  let te = infer ctx e in
                  if conv ctx (Eq (x, y)) te then
                    (* Tout va bien *)
                    normalize ctx (App (App (App (p, x), y), e))
                  else err ("Invalid type for e : " ^ to_string te)
                else err ("Invalid return type for r : " ^ to_string ro)
              else err ("Invalid input type for r : " ^ to_string tx')
          | _ -> err ("Invalid type for r : " ^ to_string tr))
      | _ -> err ("Invalid type for p (1) : " ^ to_string tp))

and check ctx e t =
  let it = infer ctx e in
  if not (conv ctx it t) then
    raise
      (Type_error
          ("Inferred type (" ^ to_string it ^ ") doesn't match expected type ("
        ^ to_string t ^ ")."))


(* Question 5.11 *)

(* copie du fichier de l'énoncé *)
let () =
  let env = ref [] in
  let loop = ref true in
  let file = open_out "interactive.proof" in
  let split c s =
    try
      let n = String.index s c in
      String.trim (String.sub s 0 n), String.trim (String.sub s (n+1) (String.length s - (n+1)))
    with Not_found -> s, ""
  in
  while !loop do
    try
      print_string "? ";
      flush_all ();
      let cmd, arg =
        let cmd = input_line stdin in
        output_string file (cmd^"\n");
        print_endline cmd;
        split ' ' cmd
      in
      match cmd with
      | "assume" ->
        let x, sa = split ':' arg in
        let a = of_string sa in
        check !env a Type;
        env := (x,(a,None)) :: !env;
        print_endline (x^" assumed of type "^to_string a)
      | "define" ->
        let x, st = split '=' arg in
        let t = of_string st in
        let a = infer !env t in
        env := (x,(a,Some t)) :: !env;
        print_endline (x^" defined to "^to_string t^" of type "^to_string a)
      | "context" ->
        print_endline (string_of_context !env)
      | "type" ->
        let t = of_string arg in
        let a = infer !env t in
        print_endline (to_string t^" is of type "^to_string a)
      | "check" ->
        let t, a = split '=' arg in
        let t = of_string t in
        let a = of_string a in
        check !env t a;
        print_endline "Ok."
      | "eval" ->
        let t = of_string arg in
        let _ = infer !env t in
        print_endline (to_string (normalize !env t))
      | "exit" -> loop := false
      | "" | "#" -> ()
      | cmd -> print_endline ("Unknown command: "^cmd)
    with
    | End_of_file -> loop := false
    | Failure err -> print_endline ("Error: "^err^".")
    | Type_error err -> print_endline ("Typing error :"^err^".")
    | Parsing.Parse_error -> print_endline ("Parsing error.")
  done;
  print_endline "Bye."