open Ds

type closure = { entry : Id.l; actual_fv : Id.t list }

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
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
  | ExtTuple of Id.l

type fundef = {
  name : Id.l * Type.t;
  args : (Id.t * Type.t) list;
  formal_fv : (Id.t * Type.t) list;
  body : t;
}

type prog = Prog of fundef list * t

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
  | MakeCls ((x, t), { entry = l; actual_fv = ys }, e) ->
      set_remove x (set_union ys (fv e))
  | AppCls (x, ys) -> x :: ys
  | AppDir (_, xs) | Tuple xs -> xs
  | LetTuple (xts, y, e) -> set_add y (set_diff (fv e) (List.map fst xts))
  | Put (x, y, z) -> [ x; y; z ]

let toplevel : fundef list ref = ref []

let rec g env known = function
  | Normalize.Unit -> Unit
  | Normalize.Int i -> Int i
  | Normalize.Float d -> Float d
  | Normalize.Neg x -> Neg x
  | Normalize.Add (x, y) -> Add (x, y)
  | Normalize.Sub (x, y) -> Sub (x, y)
  | Normalize.Mul (x, y) -> Mul (x, y)
  | Normalize.Div (x, y) -> Div (x, y)
  | Normalize.FNeg x -> FNeg x
  | Normalize.FAdd (x, y) -> FAdd (x, y)
  | Normalize.FSub (x, y) -> FSub (x, y)
  | Normalize.FMul (x, y) -> FMul (x, y)
  | Normalize.FDiv (x, y) -> FDiv (x, y)
  | Normalize.IfEq (x, y, e1, e2) -> IfEq (x, y, g env known e1, g env known e2)
  | Normalize.IfLE (x, y, e1, e2) -> IfLE (x, y, g env known e1, g env known e2)
  | Normalize.Let ((x, t), e1, e2) ->
      Let ((x, t), g env known e1, g (env_add x t env) known e2)
  | Normalize.Var x -> Var x
  | Normalize.LetRec
      ({ Normalize.name = x, t; Normalize.args = yts; Normalize.body = e1 }, e2)
    ->
      let toplevel_backup = !toplevel in
      let env' = env_add x t env in
      let known' = set_add x known in
      let e1' = g (add_list yts env') known' e1 in
      let zs = set_diff (fv e1') (List.map fst yts) in
      let known', e1' =
        if zs = [] then (known', e1')
        else (
          Format.eprintf "free variable(s) %s found in function %s@."
            (Id.pp_list zs) x;
          Format.eprintf "function %s cannot be directly applied in fact@." x;
          toplevel := toplevel_backup;
          let e1' = g (add_list yts env') known e1 in
          (known, e1'))
      in
      let zs = set_diff (fv e1') (set_add x (List.map fst yts)) in
      let zts = List.map (fun z -> (z, env_find z env')) zs in
      toplevel :=
        { name = (Id.L x, t); args = yts; formal_fv = zts; body = e1' }
        :: !toplevel;
      let e2' = g env' known' e2 in
      if set_exist x (fv e2') then
        MakeCls ((x, t), { entry = Id.L x; actual_fv = zs }, e2')
      else (
        Format.eprintf "eliminating closure(s) %s@." x;
        e2')
  | Normalize.App (x, ys) when set_exist x known ->
      Format.eprintf "directly applying %s@." x;
      AppDir (Id.L x, ys)
  | Normalize.App (f, xs) -> AppCls (f, xs)
  | Normalize.Tuple xs -> Tuple xs
  | Normalize.LetTuple (xts, y, e) ->
      LetTuple (xts, y, g (add_list xts env) known e)
  | Normalize.Get (x, y) -> Get (x, y)
  | Normalize.Put (x, y, z) -> Put (x, y, z)
  | Normalize.ExtArray x -> ExtArray (Id.L x)
  | Normalize.ExtTuple x -> ExtTuple (Id.L x)
  | Normalize.ExtFunApp (x, ys) -> AppDir (Id.L x, ys)

let f e =
  toplevel := [];
  let e' = g [] [] e in
  Prog (List.rev !toplevel, e')

let rec list_equal a b =
  match (a, b) with
  | x :: xs, y :: ys -> x = y && list_equal xs ys
  | [], [] -> true
  | _, _ -> false

let rec cse_equal a b =
  match (a, b) with
  | Unit, Unit -> true
  | Int i1, Int i2 -> i1 = i2
  | Float f1, Float f2 -> f1 = f2
  | Neg t1, Neg t2 -> t1 = t2
  | Add (t1, t2), Add (t3, t4) -> t1 = t3 && t2 = t4
  | Sub (t1, t2), Sub (t3, t4) -> t1 = t3 && t2 = t4
  | FNeg t1, FNeg t2 -> t1 = t2
  | FAdd (t1, t2), FAdd (t3, t4) -> t1 = t3 && t2 = t4
  | FSub (t1, t2), FSub (t3, t4) -> t1 = t3 && t2 = t4
  | FMul (t1, t2), FMul (t3, t4) -> t1 = t3 && t2 = t4
  | FDiv (t1, t2), FDiv (t3, t4) -> t1 = t3 && t2 = t4
  | IfEq (id1, id2, t1, t2), IfEq (id3, id4, t3, t4) ->
      id1 = id3 && id2 = id4 && cse_equal t1 t3 && cse_equal t2 t4
  | IfLE (id1, id2, t1, t2), IfLE (id3, id4, t3, t4) ->
      id1 = id3 && id2 = id4 && cse_equal t1 t3 && cse_equal t2 t4
  | Let ((id1, ty1), t1, t2), Let ((id2, ty2), t3, t4) ->
      id1 = id2 && cse_equal t1 t3 && cse_equal t2 t4
  | Var t1, Var t2 -> t1 = t2
  | Tuple t1, Tuple t2 -> list_equal t1 t2
  | LetTuple (ls1, v1, t1), LetTuple (ls2, v2, t2) ->
      list_equal ls1 ls2 && v1 = v2 && cse_equal t1 t2
  | ExtArray t1, ExtArray t2 -> t1 = t2
  | _ -> false

let rec cse_find t env =
  match env with
  | (a, b) :: rest -> if cse_equal a t then b else cse_find t rest
  | [] -> raise Not_found

let rec cse_add t id env =
  match env with
  | (a, b) :: rest ->
      if cse_equal a t then (a, id) :: cse_add t id rest
      else (a, b) :: cse_add t id rest
  | [] -> [ (t, id) ]

let rec cse_g env t =
  try
    let x = cse_find t env in
    Var x
  with Not_found -> (
    match t with
    | Let ((id, ty), t1, t2) ->
        let t1' = cse_g env t1 in
        let env' = cse_add t1' id env in
        let t2' = cse_g env' t2 in
        Let ((id, ty), t1', t2')
    | IfEq (e1, e2, t1, t2) ->
        let t1' = cse_g env t1 in
        let t2' = cse_g env t2 in
        IfEq (e1, e2, t1', t2')
    | IfLE (e1, e2, t1, t2) ->
        let t1' = cse_g env t1 in
        let t2' = cse_g env t2 in
        IfLE (e1, e2, t1', t2')
    (* | LetRec (fdef, t) ->
        let name = fdef.name in
        let args = fdef.args in
        let fv = fdef.formal_fv in
        let body = fdef.body in
        let body' = cse_g env body in
        let t' = cse_g env t in
        LetRec ({ name; args; fv; body = body' }, t') *)
    | MakeCls ((id, ty), cl, t) ->
        let t' = cse_g env t in
        MakeCls ((id, ty), cl, t')
    | LetTuple (lst, var, t) ->
        let t' = cse_g env t in
        LetTuple (lst, var, t')
    | _ -> t)

let rec cse_f fdef =
  let name = fdef.name in
  let args = fdef.args in
  let formal_fv = fdef.formal_fv in
  let body = fdef.body in
  let body = cse_g [] body in
  { name; args; formal_fv; body }

(* Common subexpression elimination *)
let rec cse (Prog (fundefs, e)) =
  (* let fundefs = List.map cse_f fundefs in *)
  (* let e = cse_g [] e in *)
  Prog (fundefs, e)

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
      ^ "\n" ^ String.make indent ' ' ^ "in\n" ^ print_t t2 indent_next
  | Var t -> t
  | MakeCls ((id, ty), cl, t) -> "MakeCls\n"
  | AppCls (id, idl) ->
      "AppCls " ^ id ^ List.fold_left (fun a b -> a ^ " " ^ b) "" idl ^ "\n"
  | AppDir (Id.L id, idl) ->
      "AppDir " ^ id ^ List.fold_left (fun a b -> a ^ " " ^ b) "" idl ^ "\n"
  | Tuple tl -> "TUPLE" ^ List.fold_left (fun s t -> s ^ " " ^ t) "" tl
  | LetTuple (idl, t1, t2) ->
      "LETTUPLE " ^ "vars:"
      ^ List.fold_left
          (fun s (id, t) -> s ^ " " ^ id ^ ": " ^ Type.print t)
          "" idl
      ^ "\n" ^ t1 ^ " = " ^ print_t t2 indent_next
  | Get (t1, t2) -> "Get " ^ t1 ^ " " ^ t2
  | Put (t1, t2, t3) -> "Put " ^ t1 ^ " " ^ t2 ^ " " ^ t3
  | ExtArray t -> "ExtArray "
  | ExtTuple t -> "ExtTuple "

let print_fun d =
  let Id.L name, typ = d.name in
  let args = d.args in
  let fargs = d.formal_fv in
  let body = d.body in
  name ^ " ("
  ^ List.fold_left (fun a (l, t) -> a ^ l ^ "(" ^ Type.print t ^ ") ") "" args
  ^ ")" ^ " ("
  ^ List.fold_left (fun a (l, t) -> a ^ l ^ "(" ^ Type.print t ^ ") ") "" fargs
  ^ "): " ^ Type.print typ ^ " = " ^ print_t body 0

let print t =
  let (Prog (fundefs, e)) = t in
  List.fold_left (fun a b -> a ^ "\n[fun] " ^ print_fun b) "" fundefs
  ^ "\n[body]" ^ print_t e 0
