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
  | AppDir (Id.L x, ys) -> x :: ys
  | Tuple xs -> xs
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
