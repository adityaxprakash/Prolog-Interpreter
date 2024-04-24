open Helper

let rec simplify_arith_terms (t : term) =
  match t with
  | Num n -> Num n
  | Func ("+", [ t1; t2 ]) -> (
      let t1' = simplify_arith_terms t1 and t2' = simplify_arith_terms t2 in
      match (t1', t2') with
      | Num n1, Num n2 -> Num (n1 + n2)
      | Wildcard, _ | _, Wildcard -> Wildcard
      | _ -> raise Ill_formed)
  | Func ("-", [ t1; t2 ]) -> (
      let t1' = simplify_arith_terms t1 and t2' = simplify_arith_terms t2 in
      match (t1', t2') with
      | Num n1, Num n2 -> Num (n1 - n2)
      | Wildcard, _ | _, Wildcard -> Wildcard
      | _ -> raise Ill_formed)
  | Func ("*", [ t1; t2 ]) -> (
      let t1' = simplify_arith_terms t1 and t2' = simplify_arith_terms t2 in
      match (t1', t2') with
      | Num n1, Num n2 -> Num (n1 * n2)
      | Wildcard, _ | _, Wildcard -> Wildcard
      | _ -> raise Ill_formed)
  | Func ("/", [ t1; t2 ]) -> (
      let t1' = simplify_arith_terms t1 and t2' = simplify_arith_terms t2 in
      match (t1', t2') with
      | Num n1, Num n2 -> Num (n1 / n2)
      | Wildcard, _ | _, Wildcard -> Wildcard
      | _ -> raise Ill_formed)
  | Func ("_mod", [ t1; t2 ]) -> (
      let t1' = simplify_arith_terms t1 and t2' = simplify_arith_terms t2 in
      match (t1', t2') with
      | Num n1, Num n2 -> Num (n1 mod n2)
      | Wildcard, _ | _, Wildcard -> Wildcard
      | _ -> raise Ill_formed)
  | Wildcard -> Wildcard
  | Var v -> Var v
  | Func (f, ts) -> Func (f, List.map simplify_arith_terms ts)

let solve_relational_atoms at sub =
  match at with
  | Not a' -> raise Ill_formed
  | Atom (op, exps) -> (
      let exps = List.map (subst_terms sub) exps in
      match op with
      | "=" -> (
          match exps with
          | [ a; b ] -> (
              try
                let unif = unify_terms a b in
                (true, unif)
              with Not_unifiable -> (false, []))
          | _ -> raise Ill_formed)
      | "\\=" -> (
          match exps with
          | [ a; b ] -> (
              try
                let _ = unify_terms a b in
                (false, [])
              with Not_unifiable -> (true, []))
          | _ -> raise Ill_formed)
      | "_is" -> (
          let exps = List.map simplify_arith_terms exps in
          match exps with
          | [ a; b ] -> (
              try
                let unif = unify_terms a b in
                (true, unif)
              with Not_unifiable -> (false, []))
          | _ -> raise Ill_formed)
      | "<" -> (
          let exps = List.map simplify_arith_terms exps in
          match exps with
          | [ Func _; _ ] | [ _; Func _ ] -> raise Ill_formed
          | [ Num n1; Num n2 ] -> (n1 < n2, sub)
          | [ Wildcard; _ ] | [ _; Wildcard ] -> (true, sub)
          | _ -> raise Ill_formed)
      | ">" -> (
          let exps = List.map simplify_arith_terms exps in
          match exps with
          | [ Func _; _ ] | [ _; Func _ ] -> raise Ill_formed
          | [ Num n1; Num n2 ] -> (n1 > n2, sub)
          | [ Wildcard; _ ] | [ _; Wildcard ] -> (true, sub)
          | _ -> raise Ill_formed)
      | "=<" -> (
          let exps = List.map simplify_arith_terms exps in
          match exps with
          | [ Func _; _ ] | [ _; Func _ ] -> raise Ill_formed
          | [ Num n1; Num n2 ] -> (n1 <= n2, sub)
          | [ Wildcard; _ ] | [ _; Wildcard ] -> (true, sub)
          | _ -> raise Ill_formed)
      | ">=" -> (
          let exps = List.map simplify_arith_terms exps in
          match exps with
          | [ Func _; _ ] | [ _; Func _ ] -> raise Ill_formed
          | [ Num n1; Num n2 ] -> (n1 >= n2, sub)
          | [ Wildcard; _ ] | [ _; Wildcard ] -> (true, sub)
          | _ -> raise Ill_formed)
      | _ -> raise Not_unifiable)

let rec answer_goal (prog' : program) (goals : goal) (subs : substitution) =
  let prog = modify_program_variables prog' in
  match goals with
  | Goal [] -> (true, subs, true)
  | Goal (g :: gs) -> (
      match g with
      | Atom ("_fail", []) | Atom ("_false", []) -> (false, [], true)
      | Atom ("_true", []) -> answer_goal prog (Goal gs) subs
      | Atom (">", e)
      | Atom ("<", e)
      | Atom ("\\=", e)
      | Atom ("=", e)
      | Atom (">=", e)
      | Atom ("=<", e)
      | Atom ("_is", e) -> (
          try
            let result = solve_relational_atoms g subs in
            if fst result then
              let new_goals = List.map (subst_atom (snd result)) gs in
              answer_goal prog (Goal new_goals)
                (compose_subst subs (snd result))
            else (false, [], true)
          with Not_unifiable | Ill_formed -> (false, [], true))
      | Not a -> (
          let subbed_a = subst_atom subs a in
          try
            let result, _, _ = answer_goal prog (Goal [ subbed_a ]) subs in
            if result then (false, [], true)
            else answer_goal prog (Goal gs) subs
          with Not_unifiable -> answer_goal prog (Goal gs) subs)
      | Atom ("_cut", []) ->
          let result, sub, _ = answer_goal prog (Goal gs) subs in
          (result, sub, false)
      | _ ->
          (* let _ =
               match g with
               | Atom (pre, ts) ->
                   print_endline ("Goal: " ^ term_to_string (Func (pre, ts)))
               | Not a -> print_string "Goal: Not"
             in *)
          let rec iterate (p : program) =
            match p with
            | [] -> (false, [], true)
            | (Head h, Body b) :: ps -> (
                try
                  let sigma = unify_atoms h g in
                  let g' = List.map (subst_atom sigma) (b @ gs)
                  and sigma' = compose_subst subs sigma in
                  let result, new_sub, not_cut =
                    answer_goal prog (Goal g') sigma'
                  in
                  if result then (result, new_sub, not_cut)
                  else if not_cut then iterate ps
                  else (false, new_sub, not_cut)
                with Not_unifiable -> iterate ps)
          in
          iterate prog)

let interpret_program (p : program) (g : goal) =
  let vars = vars_in_goal g and result, subs, _ = answer_goal p g [] in
  console_text (result, subs) vars
