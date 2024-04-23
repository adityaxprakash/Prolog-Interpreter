type term = Var of string | Num of int | Func of string * term list | Wildcard
type atom = Atom of string * term list | Not of atom
type head = Head of atom
type body = Body of atom list
type goal = Goal of atom list
type clause = head * body
type program = clause list
type substitution = (string * term) list

let true_atom = Atom ("_true", [])
let fail_atom = Atom ("_fail", [])
let cut_atom = Atom ("_cut", [])

exception Not_unifiable
exception Bad_list
exception Ill_formed

let rec union set1 set2 =
  match set1 with
  | [] -> set2
  | hd :: tl -> if List.mem hd set2 then union tl set2 else hd :: union tl set2

let rec subst_terms (s : substitution) t =
  match t with
  | Num _ -> t
  | Var x -> ( try List.assoc x s with Not_found -> t)
  | Func (f, ts) -> Func (f, List.map (subst_terms s) ts)
  | Wildcard -> Wildcard

let rec subst_atom s a =
  match a with
  | Atom (p, ts) -> Atom (p, List.map (subst_terms s) ts)
  | Not a' -> Not (subst_atom s a')

let rec is_var_in_term x t =
  match t with
  | Var y -> x = y
  | Num _ -> false
  | Func (_, ts) -> List.exists (is_var_in_term x) ts
  | Wildcard -> false

let compose_subst s1 s2 =
  let strip l = List.map (fun (x, y) -> x) l in
  let all_vars = union (strip s1) (strip s2) in
  let compose_s t = subst_terms s2 (subst_terms s1 t) in
  List.map (fun x -> (x, compose_s (Var x))) all_vars

let rec unify_terms t1 t2 : substitution =
  match (t1, t2) with
  | Num n1, Num n2 -> if n1 = n2 then [] else raise Not_unifiable
  | Wildcard, _ | _, Wildcard -> []
  | Var x, Var y -> if x = y then [] else [ (x, t2) ]
  | Var x, t | t, Var x ->
      if is_var_in_term x t then raise Not_unifiable else [ (x, t) ]
  | Func (f1, ts1), Func (f2, ts2) ->
      if f1 = f2 && List.length ts1 = List.length ts2 then
        List.fold_left2
          (fun s t1 t2 ->
            compose_subst s (unify_terms (subst_terms s t1) (subst_terms s t2)))
          [] ts1 ts2
      else raise Not_unifiable
  | _, _ -> raise Not_unifiable

let rec unify_atoms a1 a2 : substitution =
  match (a1, a2) with
  | Atom (p1, ts1), Atom (p2, ts2) ->
      unify_terms (Func (p1, ts1)) (Func (p2, ts2))
  | Not a', Not a'' -> unify_atoms a' a''
  | Not a', a | a, Not a' -> unify_atoms a' a

let rec vars_in_atom a =
  match a with
  | Atom (a, ts) ->
      let rec vars_in_term t =
        match t with
        | Var x -> [ x ]
        | Num _ -> []
        | Func (_, ts) -> List.flatten (List.map vars_in_term ts)
        | Wildcard -> []
      in
      vars_in_term (Func (a, ts))
  | Not a' -> vars_in_atom a'

let vars_in_goal g =
  match g with Goal atoms -> List.flatten (List.map vars_in_atom atoms)

let rec term_to_string t =
  match t with
  | Var x -> if x = "_underscore" then "_" else x
  | Num n -> string_of_int n
  | Func (f, ts) -> (
      match f with
      | "_empty_list" | "_list" ->
          let rec list_contents_to_string t =
            match t with
            | Func ("_empty_list", []) -> ""
            | Func ("_list", [ t1; t2 ]) -> (
                match (t1, t2) with
                | t', Func ("_empty_list", []) -> term_to_string t'
                | t', Func ("_list", r) ->
                    term_to_string t' ^ ", "
                    ^ list_contents_to_string (Func ("_list", r))
                | t', Var x -> term_to_string t' ^ "|" ^ term_to_string (Var x)
                | t', Wildcard ->
                    term_to_string t' ^ "|" ^ term_to_string Wildcard
                | _ -> raise Bad_list)
            | _ -> raise Bad_list
          in
          "[" ^ list_contents_to_string t ^ "]"
      | _ ->
          if List.length ts = 0 then f
          else f ^ "(" ^ String.concat ", " (List.map term_to_string ts) ^ ")")
  | Wildcard -> "_"

let substitution_to_string s =
  let pair_to_string (x, t) = x ^ " = " ^ term_to_string t in
  String.concat ", " (List.map pair_to_string s)

let rec modify_atom_variables a =
  let rec modify_term_variables t =
    match t with
    | Var x -> Var ("0" ^ x)
    | Num n -> Num n
    | Func (f, ts) -> Func (f, List.map modify_term_variables ts)
    | Wildcard -> Wildcard
  in
  match a with
  | Atom (p, ts) -> Atom (p, List.map modify_term_variables ts)
  | Not a' -> Not (modify_atom_variables a')

let rec modify_program_variables p =
  let rec modify_clause c =
    match c with
    | Head a, Body b ->
        (Head (modify_atom_variables a), Body (List.map modify_atom_variables b))
  in
  List.map modify_clause p

let console_text subs vars =
  let rec add_extra_vars var_list sub =
    match var_list with
    | [] -> sub
    | hd :: tl -> (
        match List.assoc_opt hd sub with
        | Some _ -> add_extra_vars tl sub
        | None -> add_extra_vars tl ((hd, Wildcard) :: sub))
  in
  let printable_subs =
    add_extra_vars vars (List.filter (fun (x, y) -> List.mem x vars) (snd subs))
  in
  (fst subs, substitution_to_string printable_subs)
