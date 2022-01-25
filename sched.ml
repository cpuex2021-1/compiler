open Ds
open Asm

let findi x env = match env_find x env with Set i -> i | _ -> raise Not_found

let findf x env = match env_find x env with SetF f -> f | _ -> raise Not_found

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

let elim e = e

let peephole e = e

let constreg e = e

let rec f e n =
  if n = 0 then e
  else
    let e' = constfold e in
    let e' = elim e' in
    let e' = peephole e' in
    let e' = constreg e' in
    if e = e' then e else f e' (n - 1)
