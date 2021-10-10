type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
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
  | ExtFunApp of Id.t * Id.t list

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec env_exists x env =
  match env with
  | (a, b) :: xs -> if a = x then true else env_exists x xs
  | [] -> false

let rec env_find x env =
  match env with
  | (a, b) :: xs -> if a = x then b else env_find x xs
  | [] -> raise Not_found

let rec env_add x t env = if env_exists x env then env else env @ [ (x, t) ]

let rec env_map f env =
  match env with (a, b) :: xs -> (a, f b) :: env_map f xs | [] -> []

let add_list xys env =
  List.fold_left (fun env (x, y) -> env_add x y env) env xys

let insert_let (exp, typ) func =
  match exp with
  | Var x -> func x
  | _ ->
      let x = Id.gentmp typ in
      let exp', typ' = func x in
      (Let ((x, typ), exp, exp'), typ')

let rec g env exp =
  match exp with
  | Syntax.Unit -> (Unit, Type.Unit)
  | Syntax.Bool b -> (Int (if b then 1 else 0), Type.Int)
  | Syntax.Int i -> (Int i, Type.Int)
  | Syntax.Float f -> (Float f, Type.Float)
  | Syntax.Not e -> g env (Syntax.If (e, Syntax.Bool false, Syntax.Bool true))
  | Syntax.Neg e -> insert_let (g env e) (fun x -> (Neg x, Type.Int))
  | Syntax.Add (e1, e2) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y -> (Add (x, y), Type.Int)))
  | Syntax.Sub (e1, e2) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y -> (Sub (x, y), Type.Int)))
  | Syntax.FNeg e -> insert_let (g env e) (fun x -> (Neg x, Type.Float))
  | Syntax.FAdd (e1, e2) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y -> (FAdd (x, y), Type.Float)))
  | Syntax.FSub (e1, e2) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y -> (FSub (x, y), Type.Float)))
  | Syntax.FMul (e1, e2) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y -> (FMul (x, y), Type.Float)))
  | Syntax.FDiv (e1, e2) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y -> (FDiv (x, y), Type.Float)))
  | Syntax.Eq (e1, e2) ->
      g env
        (Syntax.If (Syntax.Eq (e1, e2), Syntax.Bool true, Syntax.Bool false))
  | Syntax.LE (e1, e2) ->
      g env
        (Syntax.If (Syntax.LE (e1, e2), Syntax.Bool true, Syntax.Bool false))
  | Syntax.If (Syntax.Not e1, e2, e3) -> g env (Syntax.If (e1, e3, e2))
  | Syntax.If (Syntax.Eq (e1, e2), e3, e4) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              (IfEq (x, y, e3', e4'), t3)))
  | Syntax.If (Syntax.LE (e1, e2), e3, e4) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              (IfLE (x, y, e3', e4'), t3)))
  | Syntax.If (e1, e2, e3) ->
      g env (Syntax.If (Syntax.Eq (e1, Syntax.Bool true), e2, e3))
  | Syntax.Let ((x, t), e2, e3) ->
      let e2', t2 = g env e2 in
      let e3', t3 = g (env_add x t env) e3 in
      (Let ((x, t), e2', e3'), t2)
  | Syntax.Var x -> (
      if env_exists x env then (Var x, env_find x env)
      else
        match env_find x !TypeCheck.extenv with
        | Type.Array e -> (ExtArray x, Type.Array e)
        | _ ->
            failwith
              (Printf.sprintf "external variable %s does not have an array type"
                 x))
  | Syntax.LetRec
      ({ Syntax.name = x, t; Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = env_add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (add_list yts env') e1 in
      (LetRec ({ name = (x, t); args = yts; body = e1' }, e2'), t2)
  | Syntax.App (Syntax.Var f, e2s) when not (env_exists f env) -> (
      match env_find f !TypeCheck.extenv with
      | Type.Fun (_, t) ->
          let rec bind xs es =
            match es with
            | [] -> (ExtFunApp (f, xs), t)
            | e2 :: e2s ->
                insert_let (g env e2) (fun x -> bind (xs @ [ x ]) e2s)
          in
          bind [] e2s
      | _ -> assert false)
  | Syntax.App (e1, e2s) -> (
      match g env e1 with
      | (_, Type.Fun (_, t)) as g_e1 ->
          insert_let g_e1 (fun f ->
              let rec bind xs es =
                match es with
                | [] -> (ExtFunApp (f, xs), t)
                | e2 :: e2s ->
                    insert_let (g env e2) (fun x -> bind (xs @ [ x ]) e2s)
              in
              bind [] e2s)
      | _ -> assert false)
  | Syntax.Tuple es ->
      let rec bind xs ts es =
        match es with
        | [] -> (Tuple xs, Type.Tuple ts)
        | e :: es ->
            let ex, tx = g env e in
            insert_let (ex, tx) (fun x -> bind (xs @ [ x ]) (ts @ [ tx ]) es)
      in
      bind [] [] es
  | Syntax.LetTuple (xts, e1, e2) ->
      insert_let (g env e1) (fun y ->
          let e2', t2 = g (add_list xts env) e2 in
          (LetTuple (xts, y, e2'), t2))
  | Syntax.Array (e1, e2) ->
      insert_let (g env e1) (fun x ->
          let ((_, t2) as g_e2) = g env e2 in
          insert_let g_e2 (fun y ->
              let l =
                match t2 with
                | Type.Float -> "create_float_array"
                | _ -> "create_array"
              in
              (ExtFunApp (l, [ x; y ]), Type.Array t2)))
  | Syntax.Get (e1, e2) -> (
      match g env e1 with
      | (_, Type.Array t) as g_e1 ->
          insert_let g_e1 (fun x ->
              insert_let (g env e2) (fun y -> (Get (x, y), t)))
      | _ -> assert false)
  | Syntax.Put (e1, e2, e3) ->
      insert_let (g env e1) (fun x ->
          insert_let (g env e2) (fun y ->
              insert_let (g env e3) (fun z -> (Put (x, y, z), Type.Unit))))

let f exp = fst (g [] exp)

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
      ^ "\n" ^ String.make indent ' ' ^ "in\n" ^ print_t t2 indent_next
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
      ^ "\n" ^ String.make indent ' ' ^ "] in\n" ^ print_t t indent_next
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
  | ExtFunApp (t, tl) ->
      "ExtFunApp " ^ t ^ List.fold_left (fun s t -> s ^ " " ^ t) "" tl

let print t = print_t t 0
