open Ds

let data = ref []

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
      match t with
      | Type.Unit -> acc
      | Type.Float -> addf acc x
      | _ -> addi acc x t)
    ini xts

let separate xts =
  classify xts ([], [])
    (fun (int, float) x -> (int, float @ [ x ]))
    (fun (int, float) x _ -> (int @ [ x ], float))

let expand xts ini addf addi =
  classify xts ini
    (fun (offset, acc) x ->
      let offset = Asm.align offset in
      (offset + 8, addf x offset acc))
    (fun (offset, acc) x t -> (offset + 4, addi x t offset acc))

let rec log2 n = if n = 1 then 0 else 1 + log2 (n / 2)

let rec g env = function
  | Closure.Unit -> Asm.Ans Asm.Nop
  | Closure.Int i -> Asm.Ans (Asm.Set i)
  | Closure.Float d ->
      let l =
        try
          let l, _ = List.find (fun (_, d') -> d = d') !data in
          l
        with Not_found ->
          let l = Id.L (Id.genid "l") in
          data := (l, d) :: !data;
          l
      in
      let x = Id.genid "l" in
      Let ((x, Type.Int), Asm.SetL l, Asm.Ans (Asm.LdDF (x, Asm.C 0)))
  | Closure.Neg x -> Asm.Ans (Asm.Neg x)
  | Closure.Add (x, y) -> Asm.Ans (Asm.Add (x, Asm.V y))
  | Closure.Sub (x, y) -> Asm.Ans (Asm.Sub (x, Asm.V y))
  | Closure.Mul (x, y) ->
      Asm.Ans (Asm.SLL (x, Asm.C 2))
      (* TODO *)
      (* let z = env_find y env in
         Asm.Ans (Asm.SLL (x, Asm.C (log2 z))) *)
  | Closure.Div (x, y) ->
      Asm.Ans (Asm.SLL (x, Asm.C (-1)))
      (* TODO *)
      (* let z = env_find y env in
         Asm.Ans (Asm.SLL (x, Asm.C (-1 * log2 z))) *)
  | Closure.FNeg x -> Asm.Ans (Asm.FNegD x)
  | Closure.FAdd (x, y) -> Asm.Ans (Asm.FAddD (x, y))
  | Closure.FSub (x, y) -> Asm.Ans (Asm.FSubD (x, y))
  | Closure.FMul (x, y) -> Asm.Ans (Asm.FMulD (x, y))
  | Closure.FDiv (x, y) -> Asm.Ans (Asm.FDivD (x, y))
  | Closure.IfEq (x, y, e1, e2) -> (
      match env_find x env with
      | Type.Int -> Asm.Ans (Asm.IfEq (x, Asm.V y, g env e1, g env e2))
      | Type.Float -> Asm.Ans (Asm.IfFEq (x, y, g env e1, g env e2))
      | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE (x, y, e1, e2) -> (
      match env_find x env with
      | Type.Int -> Asm.Ans (Asm.IfLE (x, Asm.V y, g env e1, g env e2))
      | Type.Float -> Asm.Ans (Asm.IfFLE (x, y, g env e1, g env e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let ((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (env_add x t1 env) e2 in
      Asm.concat e1' (x, t1) e2'
  | Closure.Var x -> (
      match env_find x env with
      | Type.Unit -> Asm.Ans Asm.Nop
      | Type.Float -> Asm.Ans (Asm.FMovD x)
      | _ -> Asm.Ans (Asm.Mov x))
  | Closure.MakeCls ((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2)
    ->
      let e2' = g (env_add x t env) e2 in
      let offset, store_fv =
        expand
          (List.map (fun y -> (y, env_find y env)) ys)
          (4, e2')
          (fun y offset store_fv ->
            Asm.seq (Asm.StDF (y, x, Asm.C offset), store_fv))
          (fun y _ offset store_fv ->
            Asm.seq (Asm.St (y, x, Asm.C offset), store_fv))
      in
      Asm.Let
        ( (x, t),
          Asm.Mov Asm.reg_hp,
          Asm.Let
            ( (Asm.reg_hp, Type.Int),
              Asm.Add (Asm.reg_hp, Asm.C (Asm.align offset)),
              let z = Id.genid "l" in
              Asm.Let
                ( (z, Type.Int),
                  Asm.SetL l,
                  Asm.seq (Asm.St (z, x, Asm.C 0), store_fv) ) ) )
  | Closure.AppCls (x, ys) ->
      let int, float = separate (List.map (fun y -> (y, env_find y env)) ys) in
      Asm.Ans (Asm.CallCls (x, int, float))
  | Closure.AppDir (Id.L x, ys) ->
      let int, float = separate (List.map (fun y -> (y, env_find y env)) ys) in
      Asm.Ans (Asm.CallDir (Id.L x, int, float))
  | Closure.Tuple xs ->
      let y = Id.genid "t" in
      let offset, store =
        expand
          (List.map (fun x -> (x, env_find x env)) xs)
          (0, Asm.Ans (Asm.Mov y))
          (fun x offset store -> Asm.seq (Asm.StDF (x, y, Asm.C offset), store))
          (fun x _ offset store -> Asm.seq (Asm.St (x, y, Asm.C offset), store))
      in
      Asm.Let
        ( (y, Type.Tuple (List.map (fun x -> env_find x env) xs)),
          Asm.Mov Asm.reg_hp,
          Asm.Let
            ( (Asm.reg_hp, Type.Int),
              Asm.Add (Asm.reg_hp, Asm.C (Asm.align offset)),
              store ) )
  | Closure.LetTuple (xts, y, e2) ->
      let s = Closure.fv e2 in
      let offset, load =
        expand xts
          (0, g (add_list xts env) e2)
          (fun x offset load ->
            if not (set_exist x s) then load
            else Asm.fletd (x, Asm.LdDF (y, Asm.C offset), load))
          (fun x t offset load ->
            if not (set_exist x s) then load
            else Asm.Let ((x, t), Asm.Ld (y, Asm.C offset), load))
      in
      load
  | Closure.Get (x, y) -> (
      let offset = Id.genid "o" in
      match env_find x env with
      | Type.Array Type.Unit -> Asm.Ans Asm.Nop
      | Type.Array Type.Float ->
          Asm.Let
            ( (offset, Type.Int),
              Asm.SLL (y, C 3),
              Asm.Ans (Asm.LdDF (x, Asm.V offset)) )
      | Type.Array _ ->
          Asm.Let
            ( (offset, Type.Int),
              Asm.SLL (y, Asm.C 2),
              Asm.Ans (Asm.Ld (x, Asm.V offset)) )
      | _ -> assert false)
  | Closure.Put (x, y, z) -> (
      let offset = Id.genid "o" in
      match env_find x env with
      | Type.Array Type.Unit -> Asm.Ans Asm.Nop
      | Type.Array Type.Float ->
          Asm.Let
            ( (offset, Type.Int),
              Asm.SLL (y, Asm.C 3),
              Asm.Ans (Asm.StDF (z, x, Asm.V offset)) )
      | Type.Array _ ->
          Asm.Let
            ( (offset, Type.Int),
              Asm.SLL (y, Asm.C 2),
              Asm.Ans (Asm.St (z, x, Asm.V offset)) )
      | _ -> assert false)
  | Closure.ExtArray (Id.L x) -> Asm.Ans (Asm.SetL (Id.L ("min_caml_" ^ x)))
  | Closure.ExtTuple (Id.L x) -> Asm.Ans (Asm.SetL (Id.L ("min_caml_" ^ x)))

(* 関数の仮想マシンコード生成 *)
let h
    {
      Closure.name = Id.L x, t;
      Closure.args = yts;
      Closure.formal_fv = zts;
      Closure.body = e;
    } =
  let int, float = separate yts in
  let offset, load =
    expand zts
      (4, g (env_add x t (add_list yts (add_list zts []))) e)
      (fun z offset load ->
        Asm.fletd (z, Asm.LdDF (Asm.reg_cl, Asm.C offset), load))
      (fun z t offset load ->
        Asm.Let ((z, t), Asm.Ld (Asm.reg_cl, Asm.C offset), load))
  in
  match t with
  | Type.Fun (_, t2) ->
      {
        Asm.name = Id.L x;
        Asm.args = int;
        Asm.fargs = float;
        Asm.body = load;
        Asm.ret = t2;
      }
  | _ -> assert false

let f (Closure.Prog (fundefs, e)) =
  data := [];
  let fundefs = List.map h fundefs in
  let e = g [] e in
  Asm.Prog (!data, fundefs, e)

let print_i id = match id with Asm.V t -> t | Asm.C i -> string_of_int i

let rec print_exp e =
  match e with
  | Asm.Nop -> "nop"
  | Asm.Set i -> "set " ^ string_of_int i
  | Asm.SetL (L l) -> "setl " ^ l
  | Asm.Mov t -> "mov " ^ t
  | Asm.Neg t -> "neg " ^ t
  | Asm.Add (t, i) -> "add " ^ t ^ " " ^ print_i i
  | Asm.Sub (t, i) -> "sub " ^ t ^ " " ^ print_i i
  | Asm.SLL (t, i) -> "sll " ^ t ^ " " ^ print_i i
  | Asm.Ld (t, i) -> "ld " ^ t ^ " " ^ print_i i
  | Asm.St (t1, t2, i) -> "st " ^ t1 ^ " " ^ t2 ^ " " ^ print_i i
  | Asm.FMovD t -> "fmovd " ^ t
  | Asm.FNegD t -> "fnegd " ^ t
  | Asm.FAddD (t1, t2) -> "faddd " ^ t1 ^ " " ^ t2
  | Asm.FSubD (t1, t2) -> "fsubd " ^ t1 ^ " " ^ t2
  | Asm.FMulD (t1, t2) -> "fmuld " ^ t1 ^ " " ^ t2
  | Asm.FDivD (t1, t2) -> "fdivd " ^ t1 ^ " " ^ t2
  | Asm.LdDF (t, i) -> "lddf " ^ t ^ " " ^ print_i i
  | Asm.StDF (t1, t2, i) -> "stdf " ^ t1 ^ " " ^ t2 ^ " " ^ print_i i
  | Asm.Comment s -> "comment " ^ s
  | Asm.IfEq (id, i, t1, t2) ->
      "if " ^ id ^ " = " ^ print_i i ^ " then " ^ print_t t1 ^ " else "
      ^ print_t t2
  | Asm.IfLE (id, i, t1, t2) ->
      "if " ^ id ^ " <= " ^ print_i i ^ " then " ^ print_t t1 ^ " else "
      ^ print_t t2
  | Asm.IfGE (id, i, t1, t2) ->
      "if " ^ id ^ " >= " ^ print_i i ^ " then " ^ print_t t1 ^ " else "
      ^ print_t t2
  | Asm.IfFEq (id1, id2, t1, t2) ->
      "if " ^ id1 ^ " =. " ^ id2 ^ " then " ^ print_t t1 ^ " else " ^ print_t t2
  | Asm.IfFLE (id1, id2, t1, t2) ->
      "if " ^ id1 ^ " <=. " ^ id2 ^ " then " ^ print_t t1 ^ " else "
      ^ print_t t2
  | Asm.CallCls (t1, t2, t3) ->
      "callcls " ^ t1 ^ "("
      ^ List.fold_left (fun a b -> a ^ " " ^ b) "" t2
      ^ ") ("
      ^ List.fold_left (fun a b -> a ^ " " ^ b) "" t3
      ^ ")"
  | Asm.CallDir (Id.L t1, t2, t3) ->
      "calldir " ^ t1 ^ "("
      ^ List.fold_left (fun a b -> a ^ " " ^ b) "" t2
      ^ ") ("
      ^ List.fold_left (fun a b -> a ^ " " ^ b) "" t3
      ^ ")"
  | Asm.Save (t1, t2) -> "save " ^ t1 ^ " " ^ t2
  | Asm.Restore t -> "restore " ^ t

and print_t t =
  match t with
  | Asm.Ans e -> print_exp e
  | Asm.Let ((id, typ), e, t') ->
      print_endline (id ^ ": " ^ Type.print typ ^ " = " ^ print_exp e ^ " \n");
      print_t t'

let print_fun d =
  let (Id.L name) = d.Asm.name in
  let args = d.Asm.args in
  let fargs = d.Asm.fargs in
  let body = d.Asm.body in
  let ret = d.Asm.ret in
  name ^ " ("
  ^ List.fold_left (fun a b -> a ^ " " ^ b) "" args
  ^ ")" ^ " ("
  ^ List.fold_left (fun a b -> a ^ " " ^ b) "" fargs
  ^ ")" ^ " -> " ^ Type.print ret ^ " = " ^ print_t body

let print t =
  let (Asm.Prog (fl, fundefs, e)) = t in
  List.fold_left (fun a b -> a ^ "\n[fun] " ^ print_fun b) "" fundefs
  ^ "\n[body] " ^ print_t e
