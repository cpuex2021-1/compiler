open Ds
open Asm

let findi x env = match env_find x env with Set i -> i | _ -> raise Not_found

let findf x env = match env_find x env with SetF f -> f | _ -> raise Not_found

let elim_count = ref 0

let opt_count = ref 0

let rec constfold_g' e env =
  (* for Asm.exp *)
  match e with
  | Set i -> Set i
  | SetF f -> SetF f
  | Mov x when env_exists x env -> (
      match env_find x env with
      | Set i -> Set i
      (* | SetF f -> SetF f *)
      | _ -> raise Not_found)
  (* | Neg x when env_exists x env -> Set (-findi x env) *)
  | Add (x, C y) when env_exists x env -> Set (findi x env + y)
  | Sub (x, C y) when env_exists x env -> Set (findi x env - y)
  | SLL (x, C y) when env_exists x env -> Set (findi x env lsl y)
  | SRL (x, C y) when env_exists x env -> Set (findi x env lsr y)
  | Add (x, V y) when env_exists x env && env_exists y env ->
      Set (findi x env + findi y env)
  | Sub (x, V y) when env_exists x env && env_exists y env ->
      Set (findi x env - findi y env)
  | SLL (x, V y) when env_exists x env && env_exists y env ->
      Set (findi x env lsl findi y env)
  | SRL (x, V y) when env_exists x env && env_exists y env ->
      Set (findi x env lsr findi y env)
  | Ld (x, V y) when env_exists x env && env_exists y env ->
      Ld ("zero", C (findi x env + findi y env))
  | Ld (x, C y) when env_exists x env -> Ld ("zero", C (findi x env + y))
  | St (x, y, V z) when env_exists y env && env_exists z env ->
      St (x, "zero", C (findi y env + findi z env))
  | St (x, y, C z) when env_exists y env -> St (x, "zero", C (findi y env + z))
  | FMovD x when env_exists x env -> (
      match env_find x env with
      (* | Set i -> Set i *)
      | SetF f -> SetF f
      | _ -> raise Not_found)
  | FAddD (x, y) when env_exists x env && env_exists y env ->
      let f = findf x env +. findf y env in
      SetF f
  | FSubD (x, y) when env_exists x env && env_exists y env ->
      let f = findf x env -. findf y env in
      SetF f
  | FMulD (x, y) when env_exists x env && env_exists y env ->
      let f = findf x env *. findf y env in
      SetF f
  | FDivD (x, y) when env_exists x env && env_exists y env ->
      let f = findf x env /. findf y env in
      SetF f
  | LdDF (x, V y) when env_exists x env && env_exists y env ->
      LdDF ("zero", C (findi x env + findi y env))
  | LdDF (x, C y) when env_exists x env -> LdDF ("zero", C (findi x env + y))
  | StDF (x, y, V z) when env_exists y env && env_exists z env ->
      StDF (x, "zero", C (findi y env + findi z env))
  | StDF (x, y, C z) when env_exists y env ->
      StDF (x, "zero", C (findi y env + z))
  | IfEq (x, V y, e1, e2) when env_exists x env && env_exists y env ->
      let xi = findi x env in
      let yi = findi y env in
      if xi = yi then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | IfEq (x, C yi, e1, e2) when env_exists x env ->
      let xi = findi x env in
      if xi = yi then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | IfLE (x, V y, e1, e2) when env_exists x env && env_exists y env ->
      let xi = findi x env in
      let yi = findi y env in
      if xi <= yi then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | IfLE (x, C yi, e1, e2) when env_exists x env ->
      let xi = findi x env in
      if xi <= yi then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | IfGE (x, V y, e1, e2) when env_exists x env && env_exists y env ->
      let xi = findi x env in
      let yi = findi y env in
      if xi >= yi then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | IfGE (x, C yi, e1, e2) when env_exists x env ->
      let xi = findi x env in
      if xi >= yi then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | IfFEq (x, y, e1, e2) when env_exists x env && env_exists y env ->
      let xf = findf x env in
      let yf = findf y env in
      if xf = yf then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | IfFLE (x, y, e1, e2) when env_exists x env && env_exists y env ->
      let xf = findf x env in
      let yf = findf y env in
      if xf <= yf then IfEq ("zero", C 0, constfold_g e1 env, Ans Nop)
      else IfEq ("zero", C 0, constfold_g e2 env, Ans Nop)
  | e -> e

and constfold_g e env =
  (* for Asm.t *)
  match e with
  | Ans exp -> Ans (constfold_g' exp env)
  | Let ((x, t), e1, e2) -> (
      let e1' = constfold_g' e1 env in
      match e1' with
      | Set i ->
          let env' = env_add x (Set i) env in
          let e2' = constfold_g e2 env' in
          Let ((x, t), e1', e2')
      | SetF f ->
          let env' = env_add x (SetF f) env in
          let e2' = constfold_g e2 env' in
          Let ((x, t), e1', e2')
      | _ ->
          let e2' = constfold_g e2 env in
          Let ((x, t), e1', e2'))

let constfold_h { name = Id.L x; args = ys; fargs = zs; body = e; ret = t } =
  { name = Id.L x; args = ys; fargs = zs; body = constfold_g e []; ret = t }

let constfold (Prog (data, fundefs, e)) =
  Printf.fprintf stderr "[Const fold asm]\n";
  Prog (data, List.map constfold_h fundefs, constfold_g e [])

let rec effect_h com =
  match com with
  | St _ | StDF _ | CallCls _ | CallDir _ | Save _ | Restore _ -> true
  | IfEq (_, _, e1, e2) -> effect_g e1 || effect_g e2
  | IfLE (_, _, e1, e2) -> effect_g e1 || effect_g e2
  | IfGE (_, _, e1, e2) -> effect_g e1 || effect_g e2
  | IfFEq (_, _, e1, e2) -> effect_g e1 || effect_g e2
  | IfFLE (_, _, e1, e2) -> effect_g e1 || effect_g e2
  | _ -> false

and effect_g exp =
  match exp with
  | Ans com -> effect_h com
  | Let ((x, t), com, e) ->
      (effect_h com || effect_g e) || set_exist x (fv_exp com)

let rec elim_h' exp =
  match exp with
  | IfEq (x, y, e1, e2) -> IfEq (x, y, elim_g' e1, elim_g' e2)
  | IfLE (x, y, e1, e2) -> IfLE (x, y, elim_g' e1, elim_g' e2)
  | IfGE (x, y, e1, e2) -> IfGE (x, y, elim_g' e1, elim_g' e2)
  | IfFEq (x, y, e1, e2) -> IfFEq (x, y, elim_g' e1, elim_g' e2)
  | IfFLE (x, y, e1, e2) -> IfFLE (x, y, elim_g' e1, elim_g' e2)
  | x -> x

and elim_g' exp =
  match exp with
  | Ans com -> Ans (elim_h' com)
  | Let ((x, t), com, e) ->
      let e' = elim_g' e in
      let com' = elim_h' com in
      if (effect_h com' || set_exist x (fv e')) || is_reg x then
        Let ((x, t), com', e')
      else (
        elim_count := !elim_count + 1;
        e')

let elim_h { name = Id.L x; args = ys; fargs = zs; body = e; ret = t } =
  let e' = elim_g' e in
  { name = Id.L x; args = ys; fargs = zs; body = e'; ret = t }

let elim (Prog (data, fundefs, e)) =
  Printf.fprintf stderr "[Eliminate asm]\n";
  Prog (data, List.map elim_h fundefs, elim_g' e)

(* for Asm.exp *)
let rec peephole_h com =
  match com with
  | IfEq (y, C 0, e1, e2) -> IfEq (y, V "zero", peephole_g e1, peephole_g e2)
  | IfLE (y, C 0, e1, e2) -> IfLE (y, V "zero", peephole_g e1, peephole_g e2)
  | IfGE (y, C 0, e1, e2) -> IfGE (y, V "zero", peephole_g e1, peephole_g e2)
  | IfEq (y, ioi, e1, e2) ->
      let e1' = peephole_g e1 in
      let e2' = peephole_g e2 in
      IfEq (y, ioi, e1', e2')
  | IfLE (y, ioi, e1, e2) ->
      let e1' = peephole_g e1 in
      let e2' = peephole_g e2 in
      IfLE (y, ioi, e1', e2')
  | IfGE (y, ioi, e1, e2) ->
      let e1' = peephole_g e1 in
      let e2' = peephole_g e2 in
      IfGE (y, ioi, e1', e2')
  | IfFEq (y, ioi, e1, e2) ->
      let e1' = peephole_g e1 in
      let e2' = peephole_g e2 in
      IfFEq (y, ioi, e1', e2')
  | IfFLE (y, ioi, e1, e2) ->
      let e1' = peephole_g e1 in
      let e2' = peephole_g e2 in
      IfFLE (y, ioi, e1', e2')
  | Ld (y, V z) when z = "zero" ->
      opt_count := !opt_count + 1;
      Ld (y, C 0)
  | Ld (y, V z) when y = "zero" ->
      opt_count := !opt_count + 1;
      Ld (z, C 0)
  | St (x, y, V z) when z = "zero" ->
      opt_count := !opt_count + 1;
      St (x, y, C 0)
  | St (x, y, V z) when y = "zero" ->
      opt_count := !opt_count + 1;
      St (x, z, C 0)
  | StDF (x, y, V z) when z = "zero" ->
      opt_count := !opt_count + 1;
      StDF (x, y, C 0)
  | StDF (x, y, V z) when y = "zero" ->
      opt_count := !opt_count + 1;
      StDF (x, z, C 0)
  | LdDF (y, V z) when z = "zero" ->
      opt_count := !opt_count + 1;
      LdDF (y, C 0)
  | LdDF (y, V z) when y = "zero" ->
      opt_count := !opt_count + 1;
      LdDF (z, C 0)
  | _ -> com

(* for Asm.t *)
and peephole_g exp =
  match exp with
  | Let ((y1, t1), Ld (x1, i1), Let ((y2, t2), Ld (x2, C i2), e1))
    when y1 = y2 && y1 <> x2 ->
      Printf.eprintf "redundant load %s found.\n" y1;
      peephole_g (Let ((y2, t2), Ld (x2, C i2), e1))
  | Let ((y1, t1), Ld (x1, i1), Let ((y2, t2), Ld (x2, V i2), e1))
    when y1 = y2 && y1 <> x2 && y1 <> i2 ->
      Printf.eprintf "redundant load %s found.\n" y1;
      peephole_g (Let ((y2, t2), Ld (x2, V i2), e1))
  | Let ((x1, t1), Add (z, C i1), Let ((x2, t2), Add (y1, C i2), e1))
    when x1 = y1 && y1 = x2 ->
      opt_count := !opt_count + 1;
      peephole_g (Let ((x2, t2), Add (z, C (i1 + i2)), e1))
  | Let ((x1, t1), Set i1, Let ((x2, t2), Add (y1, C i2), e1))
    when x1 = y1 && y1 = x2 ->
      opt_count := !opt_count + 1;
      peephole_g (Let ((x2, t2), Set (i1 + i2), e1))
  | Let ((x, tx), Add (y, C c1), Let ((z, tz), Ld (x2, C c2), exp))
    when x = x2 && z <> x && z <> y && abs (c1 + c2) < 32768 ->
      opt_count := !opt_count + 1;
      peephole_g
        (Let ((z, tz), Ld (y, C (c1 + c2)), Let ((x, tx), Add (y, C c1), exp)))
  (* | Let ((x, tx), Add (y, C c1), Let ((z, tz), Ld (x2, C c2), exp))
     when x = x2 && z = x && z <> y && abs (c1 + c2) < 32768 ->
       opt_count := !opt_count + 1;
       peephole_g
         (Let ((z, tz), Ld (y, C (c1 + c2)), Let ((x, tx), Add (y, C c1), exp))) *)
  | Let ((x, t), com, e2) ->
      let e2' = peephole_g e2 in
      let com' = peephole_h com in
      Let ((x, t), com', e2')
  | Ans com -> Ans (peephole_h com)

let peephole_h { name = Id.L x; args = ys; fargs = zs; body = e; ret = t } =
  let e' = peephole_g e in
  { name = Id.L x; args = ys; fargs = zs; body = e'; ret = t }

let peephole (Prog (data, fundefs, e)) =
  Printf.fprintf stderr "[Peephole]\n";
  Prog (data, List.map peephole_h fundefs, peephole_g e)

let subst_ioi ioi x creg =
  match ioi with C c -> C c | V y when x = y -> V creg | V y -> V y

let subst_reg y x creg = if x = y then creg else y

let is_alias com x =
  match com with
  | Mov y when y = x -> true
  | FMovD y when y = x -> true
  | _ -> false

let rec subst_com com x creg =
  match com with
  | Mov y -> Mov (subst_reg y x creg)
  | Add (y, ioi) -> Add (subst_reg y x creg, subst_ioi ioi x creg)
  | Sub (y, ioi) -> Sub (subst_reg y x creg, subst_ioi ioi x creg)
  | SLL (y, ioi) -> SLL (subst_reg y x creg, subst_ioi ioi x creg)
  | SRL (y, ioi) -> SRL (subst_reg y x creg, subst_ioi ioi x creg)
  | Ld (y, ioi) -> Ld (subst_reg y x creg, subst_ioi ioi x creg)
  | St (y, z, ioi) ->
      St (subst_reg y x creg, subst_reg z x creg, subst_ioi ioi x creg)
  | FMovD y -> FMovD (subst_reg y x creg)
  | FAddD (y, z) -> FAddD (subst_reg y x creg, subst_reg z x creg)
  | FSubD (y, z) -> FSubD (subst_reg y x creg, subst_reg z x creg)
  | FMulD (y, z) -> FMulD (subst_reg y x creg, subst_reg z x creg)
  | FDivD (y, z) -> FDivD (subst_reg y x creg, subst_reg z x creg)
  | LdDF (y, ioi) -> LdDF (subst_reg y x creg, subst_ioi ioi x creg)
  | StDF (y, z, ioi) ->
      StDF (subst_reg y x creg, subst_reg z x creg, subst_ioi ioi x creg)
  | IfEq (y, ioi, e1, e2) ->
      IfEq
        ( subst_reg y x creg,
          subst_ioi ioi x creg,
          subst e1 x creg,
          subst e2 x creg )
  | IfLE (y, ioi, e1, e2) ->
      IfLE
        ( subst_reg y x creg,
          subst_ioi ioi x creg,
          subst e1 x creg,
          subst e2 x creg )
  | IfGE (y, ioi, e1, e2) ->
      IfGE
        ( subst_reg y x creg,
          subst_ioi ioi x creg,
          subst e1 x creg,
          subst e2 x creg )
  | IfFEq (y, z, e1, e2) ->
      IfFEq
        ( subst_reg y x creg,
          subst_reg z x creg,
          subst e1 x creg,
          subst e2 x creg )
  | IfFLE (y, z, e1, e2) ->
      IfFLE
        ( subst_reg y x creg,
          subst_reg z x creg,
          subst e1 x creg,
          subst e2 x creg )
  | CallCls (f, ys, zs) ->
      CallCls
        ( f,
          List.map (fun y -> subst_reg y x creg) ys,
          List.map (fun z -> subst_reg z x creg) zs )
  | CallDir (f, ys, zs) ->
      CallDir
        ( f,
          List.map (fun y -> subst_reg y x creg) ys,
          List.map (fun z -> subst_reg z x creg) zs )
  | Save (y, z) -> Save (subst_reg y x creg, z)
  | exp -> exp

and subst e x creg =
  match e with
  | Ans com -> Ans (subst_com com x creg)
  | Let ((y, t), com, e2) ->
      if is_alias com x then (
        Printf.eprintf "[constreg] found alias";
        subst (subst e2 y creg) x creg)
      else Let ((y, t), subst_com com x creg, subst e2 x creg)

let rec constreg_g e =
  match e with
  | Ans (Set 0) -> Ans (Mov "zero")
  | Ans (SetF f) when f = 0.0 -> Ans (FMovD "fzero")
  | Ans (IfEq (y, C 0, e1, e2)) ->
      Ans (IfEq (y, V "zero", constreg_g e1, constreg_g e2))
  | Ans (IfLE (y, C 0, e1, e2)) ->
      Ans (IfLE (y, V "zero", constreg_g e1, constreg_g e2))
  | Ans (IfGE (y, C 0, e1, e2)) ->
      Ans (IfGE (y, V "zero", constreg_g e1, constreg_g e2))
  | Ans (IfEq (y, ioi, e1, e2)) ->
      Ans (IfEq (y, ioi, constreg_g e1, constreg_g e2))
  | Ans (IfLE (y, ioi, e1, e2)) ->
      Ans (IfLE (y, ioi, constreg_g e1, constreg_g e2))
  | Ans (IfGE (y, ioi, e1, e2)) ->
      Ans (IfGE (y, ioi, constreg_g e1, constreg_g e2))
  | Ans (IfFEq (y, z, e1, e2)) ->
      Ans (IfFEq (y, z, constreg_g e1, constreg_g e2))
  | Ans (IfFLE (y, z, e1, e2)) ->
      Ans (IfFLE (y, z, constreg_g e1, constreg_g e2))
  | Ans _ -> e
  | Let ((x, t), Set 0, e2) ->
      Printf.eprintf "substitute %s for zero\n" x;
      opt_count := !opt_count + 1;
      constreg_g (subst e2 x "zero")
  | Let ((x, t), SetF f, e2) when f = 0.0 ->
      Printf.eprintf "substitute %s for fzero\n" x;
      opt_count := !opt_count + 1;
      constreg_g (subst e2 x "fzero")
  | Let ((x, t), IfEq (y, C 0, e1, e2), e3) ->
      Let
        ((x, t), IfEq (y, V "zero", constreg_g e1, constreg_g e2), constreg_g e3)
  | Let ((x, t), IfEq (y, ioi, e1, e2), e3) ->
      Let ((x, t), IfEq (y, ioi, constreg_g e1, constreg_g e2), constreg_g e3)
  | Let ((x, t), IfLE (y, C 0, e1, e2), e3) ->
      Let
        ((x, t), IfLE (y, V "zero", constreg_g e1, constreg_g e2), constreg_g e3)
  | Let ((x, t), IfLE (y, ioi, e1, e2), e3) ->
      Let ((x, t), IfLE (y, ioi, constreg_g e1, constreg_g e2), constreg_g e3)
  | Let ((x, t), IfGE (y, C 0, e1, e2), e3) ->
      Let
        ((x, t), IfGE (y, V "zero", constreg_g e1, constreg_g e2), constreg_g e3)
  | Let ((x, t), IfGE (y, ioi, e1, e2), e3) ->
      Let ((x, t), IfGE (y, ioi, constreg_g e1, constreg_g e2), constreg_g e3)
  | Let ((x, t), IfFEq (y, z, e1, e2), e3) ->
      Let ((x, t), IfFEq (y, z, constreg_g e1, constreg_g e2), constreg_g e3)
  | Let ((x, t), IfFLE (y, z, e1, e2), e3) ->
      Let ((x, t), IfFLE (y, z, constreg_g e1, constreg_g e2), constreg_g e3)
  | Let (xt, exp, e2) -> Let (xt, exp, constreg_g e2)

let constreg_h { name = Id.L x; args = ys; fargs = zs; body = e; ret = t } =
  let e' = constreg_g e in
  { name = Id.L x; args = ys; fargs = zs; body = e'; ret = t }

let constreg (Prog (data, fundefs, e)) =
  Printf.fprintf stderr "[constant register]\n";
  Prog (data, List.map constreg_h fundefs, constreg_g e)

let rec f e n =
  if n = 0 then e
  else
    let e' = constfold e in
    let e' = elim e' in
    let e' = peephole e' in
    (* let e' = constreg e' in *)
    Printf.eprintf "eliminated asm counter %d\n" !elim_count;
    Printf.eprintf "peephole optimization counter %d\n" !opt_count;
    Printf.eprintf "const reg counter %d\n" !opt_count;
    if e = e' then e else f e' (n - 1)

let rec f2 e n =
  if n = 0 then e
  else
    let e' = elim e in
    let e' = peephole e' in
    Printf.eprintf "eliminated asm counter %d\n" !elim_count;
    Printf.eprintf "peephole optimization counter %d\n" !opt_count;
    if e = e' then e else f2 e' (n - 1)
