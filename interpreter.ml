open Helper

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
      | "<" -> (
          match exps with
          | [ Func _; _ ] | [ _; Func _ ] -> raise Ill_formed
          | [ Num n1; Num n2 ] -> (n1 < n2, sub)
          | [ Wildcard; _ ] | [ _; Wildcard ] -> (true, sub)
          | _ -> raise Ill_formed)
      | ">" -> (
          match exps with
          | [ Func _; _ ] | [ _; Func _ ] -> raise Ill_formed
          | [ Num n1; Num n2 ] -> (n1 > n2, sub)
          | [ Wildcard; _ ] | [ _; Wildcard ] -> (true, sub)
          | _ -> raise Ill_formed)
      | _ -> raise Not_unifiable)

let rec answer_goal (prog' : program) (goals : goal) (subs : substitution) =
  let prog = modify_program_variables prog' in
  match goals with
  | Goal [] -> (true, subs)
  | Goal (g :: gs) -> (
      match g with
      | Atom ("_fail", []) -> (false, [])
      | Atom ("_true", []) -> answer_goal prog (Goal gs) subs
      | Atom (">", e) | Atom ("<", e) | Atom ("\\=", e) | Atom ("=", e) -> (
          try
            let result = solve_relational_atoms g subs in
            if fst result then
              answer_goal prog (Goal gs) (compose_subst subs (snd result))
            else (false, [])
          with Not_unifiable | Ill_formed -> (false, []))
      | Not a -> (
          let subbed_a = subst_atom subs a in
          try
            let result = answer_goal prog (Goal [ subbed_a ]) subs in
            if fst result then (false, []) else answer_goal prog (Goal gs) subs
          with Not_unifiable -> answer_goal prog (Goal gs) subs)
      | Atom ("_cut", []) -> answer_goal prog (Goal gs) subs
      | _ ->
          (* let _ =
               match g with
               | Atom (pre, ts) ->
                   print_endline ("Goal: " ^ term_to_string (Func (pre, ts)))
               | Not a -> print_string "Goal: Not"
             in *)
          let rec iterate (p : program) =
            match p with
            | [] -> (false, [])
            | (Head h, Body b) :: ps -> (
                try
                  let sigma = unify_atoms h g in
                  let g' = List.map (subst_atom sigma) (b @ gs)
                  and sigma' = compose_subst subs sigma in
                  let result, new_sub = answer_goal prog (Goal g') sigma' in
                  if result then (result, new_sub) else iterate ps
                with Not_unifiable ->
                  (* let (Atom (pred, ts)) = h in
                     print_endline (term_to_string (Func (pred, ts))); *)
                  iterate ps)
          in
          iterate prog)

let interpret_program (p : program) (g : goal) =
  let vars = vars_in_goal g and subs = answer_goal p g [] in
  console_text subs vars
