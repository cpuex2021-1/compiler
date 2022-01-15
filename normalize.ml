open Ds

type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Div of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtTuple of Id.t
  | ExtFunApp of Id.t * Id.t list

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec fv = function
  | Unit | Int _ | Float _ | ExtArray _ | ExtTuple _ -> []
  | Neg x | FNeg x -> [ x ]
  | Add (x, y)
  | Sub (x, y)
  | Mul (x, y)
  | Div (x, y)
  | FAdd (x, y)
  | FSub (x, y)
  | FMul (x, y)
  | FDiv (x, y)
  | Get (x, y) ->
      [ x; y ]
  | IfEq (x, y, e1, e2) | IfLE (x, y, e1, e2) ->
      set_add x (set_add y (set_union (fv e1) (fv e2)))
  | Let ((x, t), e1, e2) -> set_union (fv e1) (set_remove x (fv e2))
  | Var x -> [ x ]
  | LetRec ({ name = x, t; args = yts; body = e1 }, e2) ->
      let zs = set_diff (fv e1) (List.map fst yts) in
      set_diff (set_union zs (fv e2)) [ x ]
  | App (x, ys) -> x :: ys
  | Tuple xs | ExtFunApp (_, xs) -> xs
  | Put (x, y, z) -> [ x; y; z ]
  | LetTuple (xs, y, e) -> set_add y (set_diff (fv e) (List.map fst xs))

let insert_let (exp, typ) func =
  match exp with
  | Var x -> func x
  | _ ->
      let x = Id.gentmp typ in
      let exp', typ' = func x in
      (Let ((x, typ), exp, exp'), typ')

let rec kNorm_ env exp =
  match exp with
  | Syntax.Unit -> (Unit, Type.Unit)
  (* | Syntax.Bool b -> (Int (if b then 1 else 0), Type.Int) *)
  | Syntax.Int i -> (Int i, Type.Int)
  | Syntax.Float f -> (Float f, Type.Float)
  | Syntax.Not e -> kNorm_ env (Syntax.If (e, Syntax.Int 0, Syntax.Int 1))
  | Syntax.Neg e -> insert_let (kNorm_ env e) (fun x -> (Neg x, Type.Int))
  | Syntax.Add (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (Add (x, y), Type.Int)))
  | Syntax.Sub (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (Sub (x, y), Type.Int)))
  | Syntax.Mul (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (Mul (x, y), Type.Int)))
  | Syntax.Div (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (Div (x, y), Type.Int)))
  | Syntax.FNeg e -> insert_let (kNorm_ env e) (fun x -> (Neg x, Type.Float))
  | Syntax.FAdd (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (FAdd (x, y), Type.Float)))
  | Syntax.FSub (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (FSub (x, y), Type.Float)))
  | Syntax.FMul (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (FMul (x, y), Type.Float)))
  | Syntax.FDiv (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y -> (FDiv (x, y), Type.Float)))
  | Syntax.Eq (e1, e2) ->
      kNorm_ env (Syntax.If (Syntax.Eq (e1, e2), Syntax.Int 1, Syntax.Int 0))
  | Syntax.LE (e1, e2) ->
      kNorm_ env (Syntax.If (Syntax.LE (e1, e2), Syntax.Int 1, Syntax.Int 0))
  | Syntax.If (Syntax.Not e1, e2, e3) -> kNorm_ env (Syntax.If (e1, e3, e2))
  | Syntax.If (Syntax.Eq (e1, e2), e3, e4) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y ->
              let e3', t3 = kNorm_ env e3 in
              let e4', t4 = kNorm_ env e4 in
              (IfEq (x, y, e3', e4'), t3)))
  | Syntax.If (Syntax.LE (e1, e2), e3, e4) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y ->
              let e3', t3 = kNorm_ env e3 in
              let e4', t4 = kNorm_ env e4 in
              (IfLE (x, y, e3', e4'), t3)))
  | Syntax.If (e1, e2, e3) ->
      (* if e1 == 0 then e3 else e2 *)
      kNorm_ env (Syntax.If (Syntax.Eq (e1, Syntax.Int 0), e3, e2))
  | Syntax.Let ((x, t), e2, e3) ->
      let e2', t2 = kNorm_ env e2 in
      let e3', t3 = kNorm_ (env_add x t env) e3 in
      (Let ((x, t), e2', e3'), t2)
  | Syntax.Var x -> (
      if env_exists x env then (Var x, env_find x env)
      else
        match env_find x !TypeCheck.extenv with
        | Type.Array e -> (ExtArray x, Type.Array e)
        | Type.Tuple e -> (ExtTuple x, Type.Tuple e)
        | _ ->
            failwith
              (Printf.sprintf "external variable %s does not have an array type"
                 x))
  | Syntax.LetRec
      ({ Syntax.name = x, t; Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = env_add x t env in
      let e2', t2 = kNorm_ env' e2 in
      let e1', t1 = kNorm_ (add_list yts env') e1 in
      (LetRec ({ name = (x, t); args = yts; body = e1' }, e2'), t2)
  | Syntax.App (Syntax.Var f, e2s) when not (env_exists f env) -> (
      match env_find f !TypeCheck.extenv with
      | Type.Fun (_, t) ->
          let rec bind xs es =
            match es with
            | [] -> (ExtFunApp (f, xs), t)
            | e2 :: e2s ->
                insert_let (kNorm_ env e2) (fun x -> bind (xs @ [ x ]) e2s)
          in
          bind [] e2s
      | _ -> assert false)
  | Syntax.App (e1, e2s) -> (
      match kNorm_ env e1 with
      | (_, Type.Fun (_, t)) as g_e1 ->
          insert_let g_e1 (fun f ->
              let rec bind xs es =
                match es with
                | [] -> (App (f, xs), t)
                | e2 :: e2s ->
                    insert_let (kNorm_ env e2) (fun x -> bind (xs @ [ x ]) e2s)
              in
              bind [] e2s)
      | _ -> assert false)
  | Syntax.Tuple es ->
      let rec bind xs ts es =
        match es with
        | [] -> (Tuple xs, Type.Tuple ts)
        | e :: es ->
            let ex, tx = kNorm_ env e in
            insert_let (ex, tx) (fun x -> bind (xs @ [ x ]) (ts @ [ tx ]) es)
      in
      bind [] [] es
  | Syntax.LetTuple (xts, e1, e2) ->
      insert_let (kNorm_ env e1) (fun y ->
          let e2', t2 = kNorm_ (add_list xts env) e2 in
          (LetTuple (xts, y, e2'), t2))
  | Syntax.Array (e1, e2) ->
      insert_let (kNorm_ env e1) (fun x ->
          let ((_, t2) as g_e2) = kNorm_ env e2 in
          insert_let g_e2 (fun y ->
              let l =
                match t2 with
                | Type.Float -> "create_float_array"
                | _ -> "create_array"
              in
              (ExtFunApp (l, [ x; y ]), Type.Array t2)))
  | Syntax.Get (e1, e2) -> (
      match kNorm_ env e1 with
      | (_, Type.Array t) as g_e1 ->
          insert_let g_e1 (fun x ->
              insert_let (kNorm_ env e2) (fun y -> (Get (x, y), t)))
      | _ -> assert false)
  | Syntax.Put (e1, e2, e3) ->
      insert_let (kNorm_ env e1) (fun x ->
          insert_let (kNorm_ env e2) (fun y ->
              insert_let (kNorm_ env e3) (fun z -> (Put (x, y, z), Type.Unit))))

let kNorm exp = fst (kNorm_ [] exp)

let rec alpha_ env exp =
  match exp with
  | Unit -> Unit
  | Int i -> Int i
  | Float d -> Float d
  | Neg x -> Neg (env_find2 x env)
  | Add (x, y) -> Add (env_find2 x env, env_find2 y env)
  | Sub (x, y) -> Sub (env_find2 x env, env_find2 y env)
  | Mul (x, y) -> Mul (env_find2 x env, env_find2 y env)
  | Div (x, y) -> Div (env_find2 x env, env_find2 y env)
  | FNeg x -> FNeg (env_find2 x env)
  | FAdd (x, y) -> FAdd (env_find2 x env, env_find2 y env)
  | FSub (x, y) -> FSub (env_find2 x env, env_find2 y env)
  | FMul (x, y) -> FMul (env_find2 x env, env_find2 y env)
  | FDiv (x, y) -> FDiv (env_find2 x env, env_find2 y env)
  | IfEq (x, y, e1, e2) ->
      IfEq (env_find2 x env, env_find2 y env, alpha_ env e1, alpha_ env e2)
  | IfLE (x, y, e1, e2) ->
      IfLE (env_find2 x env, env_find2 y env, alpha_ env e1, alpha_ env e2)
  | Let ((x, t), e1, e2) ->
      let x' = Id.genid x in
      Let ((x', t), alpha_ env e1, alpha_ (env_add x x' env) e2)
  | Var x -> Var (env_find2 x env)
  | LetRec ({ name = x, t; args = yts; body = e1 }, e2) ->
      let env = env_add x (Id.genid x) env in
      let ys = List.map fst yts in
      let env' = add_list2 ys (List.map Id.genid ys) env in
      LetRec
        ( {
            name = (env_find2 x env, t);
            args = List.map (fun (y, t) -> (env_find2 y env', t)) yts;
            body = alpha_ env' e1;
          },
          alpha_ env e2 )
  | App (x, ys) -> App (env_find2 x env, List.map (fun y -> env_find2 y env) ys)
  | Tuple xs -> Tuple (List.map (fun x -> env_find2 x env) xs)
  | LetTuple (xts, y, e) ->
      let xs = List.map fst xts in
      let env' = add_list2 xs (List.map Id.genid xs) env in
      LetTuple
        ( List.map (fun (x, t) -> (env_find2 x env', t)) xts,
          env_find2 y env,
          alpha_ env' e )
  | Get (x, y) -> Get (env_find2 x env, env_find2 y env)
  | Put (x, y, z) -> Put (env_find2 x env, env_find2 y env, env_find2 z env)
  | ExtArray x -> ExtArray x
  | ExtTuple x -> ExtTuple x
  | ExtFunApp (x, ys) ->
      ExtFunApp (env_find2 x env, List.map (fun y -> env_find2 y env) ys)

let alpha exp = alpha_ [] exp

let rec print_env env =
  match env with
  | (key, value) :: rest ->
      print_endline (key ^ Type.print value);
      print_env rest
  | [] -> print_endline ""

let rec print_t t indent =
  let indent_next = indent + 2 in
  String.make indent ' '
  ^
  match t with
  | Unit -> "UNIT"
  | Int i -> "INT(" ^ string_of_int i ^ ")"
  | Float f -> "FLOAT(" ^ string_of_float f ^ ")"
  | Neg t -> "NEG " ^ t
  | Add (t1, t2) -> t1 ^ " + " ^ t2
  | Sub (t1, t2) -> t1 ^ " - " ^ t2
  | Mul (t1, t2) -> t1 ^ " * " ^ t2
  | Div (t1, t2) -> t1 ^ " / " ^ t2
  | FNeg t -> "FNEG " ^ t
  | FAdd (t1, t2) -> t1 ^ " +. " ^ t2
  | FSub (t1, t2) -> t1 ^ " -. " ^ t2
  | FMul (t1, t2) -> t1 ^ " *. " ^ t2
  | FDiv (t1, t2) -> t1 ^ " /. " ^ t2
  | IfEq (e1, e2, t1, t2) ->
      "If " ^ e1 ^ " = " ^ e2 ^ " then\n" ^ print_t t1 indent_next ^ "\n"
      ^ String.make indent ' ' ^ "else\n" ^ print_t t2 indent_next
  | IfLE (e1, e2, t1, t2) ->
      "If " ^ e1 ^ " <= " ^ e2 ^ " then\n" ^ print_t t1 indent_next ^ "\n"
      ^ String.make indent ' ' ^ "else\n" ^ print_t t2 indent_next
  | Let ((id, ty), t1, t2) ->
      "LET " ^ id ^ ": " ^ Type.print ty ^ " = \n" ^ print_t t1 indent_next
      ^ "\n" ^ String.make indent ' ' ^ "in\n" ^ print_t t2 indent
  | Var t -> t
  | LetRec (fdef, t) ->
      "LETREC " ^ "name[" ^ fst fdef.name ^ "]: ("
      ^ Type.print (snd fdef.name)
      ^ "), args["
      ^ List.fold_left
          (fun s arg -> s ^ fst arg ^ ": " ^ Type.print (snd arg) ^ ", ")
          "" fdef.args
      ^ "], body[\n"
      ^ print_t fdef.body indent_next
      ^ "\n" ^ String.make indent ' ' ^ "] in\n" ^ print_t t indent
  | App (t, tl) -> "APP " ^ t ^ List.fold_left (fun s t -> s ^ " " ^ t) "" tl
  | Tuple tl -> "TUPLE" ^ List.fold_left (fun s t -> s ^ " " ^ t) "" tl
  | LetTuple (idl, t1, t2) ->
      "LETTUPLE " ^ "vars:"
      ^ List.fold_left
          (fun s (id, t) -> s ^ " " ^ id ^ ": " ^ Type.print t)
          "" idl
      ^ "\n" ^ t1 ^ " = " ^ print_t t2 indent_next
  | Get (t1, t2) -> "Get " ^ t1 ^ " " ^ t2
  | Put (t1, t2, t3) -> "Put " ^ t1 ^ " " ^ t2 ^ " " ^ t3
  | ExtArray t -> "ExtArray " ^ t
  | ExtTuple t -> "ExtTuple " ^ t
  | ExtFunApp (t, tl) ->
      "ExtFunApp " ^ t ^ List.fold_left (fun s t -> s ^ " " ^ t) "" tl

let print t = print_t t 0
