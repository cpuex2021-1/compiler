open Normalize
open Ds

let find x env = try env_find x env with Not_found -> x

let rec beta_g env = function
  | Unit -> Unit
  | Int i -> Int i
  | Float d -> Float d
  | Neg x -> Neg (find x env)
  | Add (x, y) -> Add (find x env, find y env)
  | Sub (x, y) -> Sub (find x env, find y env)
  | Mul (x, y) -> Mul (find x env, find y env)
  | Div (x, y) -> Div (find x env, find y env)
  | FNeg x -> FNeg (find x env)
  | FAdd (x, y) -> FAdd (find x env, find y env)
  | FSub (x, y) -> FSub (find x env, find y env)
  | FMul (x, y) -> FMul (find x env, find y env)
  | FDiv (x, y) -> FDiv (find x env, find y env)
  | IfEq (x, y, e1, e2) ->
      IfEq (find x env, find y env, beta_g env e1, beta_g env e2)
  | IfLE (x, y, e1, e2) ->
      IfLE (find x env, find y env, beta_g env e1, beta_g env e2)
  | Let ((x, t), e1, e2) -> (
      match beta_g env e1 with
      | Var y ->
          Format.eprintf "beta-reducing %s = %s@." x y;
          beta_g (env_add x y env) e2
      | e1' ->
          let e2' = beta_g env e2 in
          Let ((x, t), e1', e2'))
  | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec ({ name = xt; args = yts; body = beta_g env e1 }, beta_g env e2)
  | Var x -> Var (find x env)
  | Tuple xs -> Tuple (List.map (fun x -> find x env) xs)
  | LetTuple (xts, y, e) -> LetTuple (xts, find y env, beta_g env e)
  | Get (x, y) -> Get (find x env, find y env)
  | Put (x, y, z) -> Put (find x env, find y env, find z env)
  | App (g, xs) -> App (find g env, List.map (fun x -> find x env) xs)
  | ExtArray x -> ExtArray x
  | ExtTuple x -> ExtTuple x
  | ExtFunApp (x, ys) -> ExtFunApp (x, List.map (fun y -> find y env) ys)

let beta = beta_g []

let rec assoc = function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, assoc e1, assoc e2)
  | IfLE (x, y, e1, e2) -> IfLE (x, y, assoc e1, assoc e2)
  | Let (xt, e1, e2) ->
      let rec insert = function
        | Let (yt, e3, e4) -> Let (yt, e3, insert e4)
        | LetRec (fundefs, e) -> LetRec (fundefs, insert e)
        | LetTuple (yts, z, e) -> LetTuple (yts, z, insert e)
        | e -> Let (xt, e, assoc e2)
      in
      insert (assoc e1)
  | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec ({ name = xt; args = yts; body = assoc e1 }, assoc e2)
  | LetTuple (xts, y, e) -> LetTuple (xts, y, assoc e)
  | e -> e

let threshold = ref 0

let rec size = function
  | IfEq (_, _, e1, e2)
  | IfLE (_, _, e1, e2)
  | Let (_, e1, e2)
  | LetRec ({ body = e1 }, e2) ->
      1 + size e1 + size e2
  | LetTuple (_, _, e) -> 1 + size e
  | _ -> 1

let rec inline_g env = function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, inline_g env e1, inline_g env e2)
  | IfLE (x, y, e1, e2) -> IfLE (x, y, inline_g env e1, inline_g env e2)
  | Let (xt, e1, e2) -> Let (xt, inline_g env e1, inline_g env e2)
  | LetRec ({ name = x, t; args = yts; body = e1 }, e2) ->
      let env = if size e1 > !threshold then env else env_add x (yts, e1) env in
      LetRec
        ({ name = (x, t); args = yts; body = inline_g env e1 }, inline_g env e2)
  | App (x, ys) when env_exists x env ->
      let zs, e = env_find x env in
      Format.eprintf "inlining %s@." x;
      let env' =
        List.fold_left2 (fun env' (z, t) y -> env_add z y env') [] zs ys
      in
      alpha_ env' e
  | LetTuple (xts, y, e) -> LetTuple (xts, y, inline_g env e)
  | e -> e

let inline e = inline_g [] e

let memi x env =
  try match env_find x env with Int _ -> true | _ -> false
  with Not_found -> false

let memf x env =
  try match env_find x env with Float _ -> true | _ -> false
  with Not_found -> false

let memt x env =
  try match env_find x env with Tuple _ -> true | _ -> false
  with Not_found -> false

let findi x env = match env_find x env with Int i -> i | _ -> raise Not_found

let findf x env =
  match env_find x env with Float d -> d | _ -> raise Not_found

let findt x env =
  match env_find x env with Tuple ys -> ys | _ -> raise Not_found

let rec constfold_g env = function
  | Var x when memi x env -> Int (findi x env)
  | Neg x when memi x env -> Int (-findi x env)
  | Add (x, y) when memi x env && memi y env -> Int (findi x env + findi y env)
  | Sub (x, y) when memi x env && memi y env -> Int (findi x env - findi y env)
  | FNeg x when memf x env -> Float (-.findf x env)
  | FAdd (x, y) when memf x env && memf y env ->
      Float (findf x env +. findf y env)
  | FSub (x, y) when memf x env && memf y env ->
      Float (findf x env -. findf y env)
  | FMul (x, y) when memf x env && memf y env ->
      Float (findf x env *. findf y env)
  | FDiv (x, y) when memf x env && memf y env ->
      Float (findf x env /. findf y env)
  | IfEq (x, y, e1, e2) when memi x env && memi y env ->
      if findi x env = findi y env then constfold_g env e1
      else constfold_g env e2
  | IfEq (x, y, e1, e2) when memf x env && memf y env ->
      if findf x env = findf y env then constfold_g env e1
      else constfold_g env e2
  | IfEq (x, y, e1, e2) -> IfEq (x, y, constfold_g env e1, constfold_g env e2)
  | IfLE (x, y, e1, e2) when memi x env && memi y env ->
      if findi x env <= findi y env then constfold_g env e1
      else constfold_g env e2
  | IfLE (x, y, e1, e2) when memf x env && memf y env ->
      if findf x env <= findf y env then constfold_g env e1
      else constfold_g env e2
  | IfLE (x, y, e1, e2) -> IfLE (x, y, constfold_g env e1, constfold_g env e2)
  | Let ((x, t), e1, e2) ->
      let e1' = constfold_g env e1 in
      let e2' = constfold_g (env_add x e1' env) e2 in
      Let ((x, t), e1', e2')
  | LetRec ({ name = x; args = ys; body = e1 }, e2) ->
      LetRec
        ({ name = x; args = ys; body = constfold_g env e1 }, constfold_g env e2)
  | LetTuple (xts, y, e) when memt y env ->
      List.fold_left2
        (fun e' xt z -> Let (xt, Var z, e'))
        (constfold_g env e) xts (findt y env)
  | LetTuple (xts, y, e) -> LetTuple (xts, y, constfold_g env e)
  | e -> e

let constfold = constfold_g []

let rec effect = function
  | Let (_, e1, e2) | IfEq (_, _, e1, e2) | IfLE (_, _, e1, e2) ->
      effect e1 || effect e2
  | LetRec (_, e) | LetTuple (_, _, e) -> effect e
  | App _ | Put _ | ExtFunApp _ -> true
  | _ -> false

let rec elim = function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, elim e1, elim e2)
  | IfLE (x, y, e1, e2) -> IfLE (x, y, elim e1, elim e2)
  | Let ((x, t), e1, e2) ->
      let e1' = elim e1 in
      let e2' = elim e2 in
      if effect e1' || set_exist x (fv e2') then Let ((x, t), e1', e2')
      else (
        Format.eprintf "eliminating variable %s@." x;
        e2')
  | LetRec ({ name = x, t; args = yts; body = e1 }, e2) ->
      let e2' = elim e2 in
      if set_exist x (fv e2') then
        LetRec ({ name = (x, t); args = yts; body = elim e1 }, e2')
      else (
        Format.eprintf "eliminating function %s@." x;
        e2')
  | LetTuple (xts, y, e) ->
      let xs = List.map fst xts in
      let e' = elim e in
      let live = fv e' in
      if List.exists (fun x -> set_exist x live) xs then LetTuple (xts, y, e')
      else (
        Format.eprintf "eliminating variables %s@." (Id.pp_list xs);
        e')
  | e -> e

let rec f e n =
  if n = 0 then e
  else
    let e' = beta e in
    let e' = assoc e' in
    let e' = inline e' in
    let e' = constfold e' in
    (* let e' = elim e' in *)
    if e = e' then e else f e' (n - 1)
