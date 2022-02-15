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
      (offset + 4, addf x offset acc))
    (fun (offset, acc) x t -> (offset + 1, addi x t offset acc))

let rec log2 n = if n = 1 then 0 else 1 + log2 (n / 2)

let rec rename_global l =
  match l with
  | [] -> []
  | y :: rest ->
      if List.mem_assoc y !Normalize.global_arrays then
        let addr, _ = List.assoc y !Normalize.global_arrays in
        let yt = Id.genid "g" in
        (yt, addr) :: rename_global rest
      else (y, -1) :: rename_global rest

let rec g env = function
  | Closure.Unit -> Asm.Ans Asm.Nop
  | Closure.Int i -> Asm.Ans (Asm.Set i)
  | Closure.Float d ->
      Asm.Ans (Asm.SetF d)
      (* let l =
           try
             let l, _ = List.find (fun (_, d') -> d = d') !data in
             l
           with Not_found ->
             let l = Id.L (Id.genid "l") in
             data := (l, d) :: !data;
             l
         in
         let x = Id.genid "l" in
         Let ((x, Type.Int), Asm.SetL l, Asm.Ans (Asm.LdDF (x, Asm.C 0))) *)
  | Closure.Neg x -> Asm.Ans (Asm.Neg x)
  | Closure.Add (x, y) -> Asm.Ans (Asm.Add (x, Asm.V y))
  | Closure.Sub (x, y) -> Asm.Ans (Asm.Sub (x, Asm.V y))
  | Closure.Mul (x, y) ->
      Asm.Ans (Asm.SLL (x, Asm.C 2))
      (* TODO *)
      (* let z = env_find y env in
         Asm.Ans (Asm.SLL (x, Asm.C (log2 z))) *)
  | Closure.Div (x, y) ->
      Asm.Ans (Asm.SRL (x, Asm.C 1))
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
  | Closure.Var x ->
      if env_exists x env then
        match env_find x env with
        | Type.Unit -> Asm.Ans Asm.Nop
        | Type.Float -> Asm.Ans (Asm.FMovD x)
        | _ -> Asm.Ans (Asm.Mov x)
      else
        let addr, ty = List.assoc x !Normalize.global_arrays in
        Asm.Ans (Asm.Set addr)
  | Closure.MakeCls ((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2)
    ->
      let e2' = g (env_add x t env) e2 in
      let offset, store_fv =
        expand
          (List.map (fun y -> (y, env_find y env)) ys)
          (1, e2')
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
  | Closure.AppDir (Id.L "fiszero", [ y ]) -> Asm.Ans (Asm.Fiszero y)
  | Closure.AppDir (Id.L "fispos", [ y ]) -> Asm.Ans (Asm.Fispos y)
  | Closure.AppDir (Id.L "fisneg", [ y ]) -> Asm.Ans (Asm.Fisneg y)
  | Closure.AppDir (Id.L "fneg", [ y ]) -> Asm.Ans (Asm.Fneg y)
  | Closure.AppDir (Id.L "fabs", [ y ]) -> Asm.Ans (Asm.Fabs y)
  | Closure.AppDir (Id.L "fless", [ y; z ]) -> Asm.Ans (Asm.Fless (y, z))
  | Closure.AppDir (Id.L "floor", [ y ]) -> Asm.Ans (Asm.Floor y)
  | Closure.AppDir (Id.L "fhalf", [ y ]) ->
      let z = Id.genid "h" in
      Asm.Let ((z, Type.Float), Asm.SetF 0.5, Asm.Ans (Asm.FMulD (y, z)))
  | Closure.AppDir (Id.L "int_of_float", [ y ]) -> Asm.Ans (Asm.IntOfFloat y)
  | Closure.AppDir (Id.L "float_of_int", [ y ]) -> Asm.Ans (Asm.FloatOfInt y)
  | Closure.AppDir (Id.L "sqrt", [ y ]) -> Asm.Ans (Asm.Sqrt y)
  | Closure.AppDir (Id.L "fsqr", [ y ]) -> Asm.Ans (Asm.Fsqr y)
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
  | Closure.GlobalTuple (addr, xs) ->
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
          Asm.Set addr,
          store )
  | Closure.LetTuple (xts, y, e2) ->
      let s = Closure.fv e2 in
      let env' =
        match env_exists y env with
        | false -> env_add y Type.Int (add_list xts env)
        | true -> add_list xts env
      in
      if List.mem_assoc y !Normalize.global_arrays then
        let y' = Id.genid "lt" in
        let addr, ty = List.assoc y !Normalize.global_arrays in
        let offset, load =
          expand xts
            (0, g env' e2)
            (fun x offset load ->
              if not (set_exist x s) then load
              else Asm.fletd (x, LdDF (y', Asm.C offset), load))
            (fun x t offset load ->
              if not (set_exist x s) then load
              else Asm.Let ((x, t), Ld (y', Asm.C offset), load))
        in
        Asm.Let ((y', Type.Int), Asm.Set addr, load)
      else
        let offset, load =
          expand xts
            (0, g env' e2)
            (fun x offset load ->
              if not (set_exist x s) then load
              else Asm.fletd (x, Asm.LdDF (y, Asm.C offset), load))
            (fun x t offset load ->
              if not (set_exist x s) then load
              else Asm.Let ((x, t), Asm.Ld (y, Asm.C offset), load))
        in
        load
  | Closure.Get (x, y) -> (
      if
        (* let offset = Id.genid "o" in *)
        List.mem_assoc x !Normalize.global_arrays
      then
        let addr, ty = List.assoc x !Normalize.global_arrays in
        let offset = Id.genid "o" in
        match ty with
        | Type.Float ->
            Asm.Let
              ( (offset, Type.Int),
                Asm.Set addr,
                Asm.Ans (Asm.LdDF (y, Asm.V offset)) )
        | Type.Int ->
            Asm.Let
              ( (offset, Type.Int),
                Asm.Set addr,
                Asm.Ans (Asm.Ld (y, Asm.V offset)) )
        | _ -> assert false
      else
        match env_find x env with
        | Type.Array Type.Unit -> Asm.Ans Asm.Nop
        | Type.Array Type.Float -> Asm.Ans (Asm.LdDF (x, Asm.V y))
        | Type.Array _ -> Asm.Ans (Asm.Ld (x, Asm.V y))
        | _ -> assert false)
  | Closure.Put (x, y, z) -> (
      if
        (* let offset = Id.genid "o" in *)
        List.mem_assoc x !Normalize.global_arrays
      then
        if List.mem_assoc z !Normalize.global_arrays then
          let addr, ty = List.assoc z !Normalize.global_arrays in
          let z' = Id.genid "g" in
          let offset = Id.genid "o" in
          Asm.Let
            ( (z', Type.Int),
              Asm.Set addr,
              let addr', ty' = List.assoc x !Normalize.global_arrays in
              match ty' with
              | Type.Float ->
                  Asm.Let
                    ( (offset, Type.Int),
                      Asm.Set addr',
                      Asm.Ans (Asm.StDF (z', y, Asm.V offset)) )
              | Type.Int ->
                  Asm.Let
                    ( (offset, Type.Int),
                      Asm.Set addr',
                      Asm.Ans (Asm.St (z', y, Asm.V offset)) )
              | _ -> assert false )
        else
          let addr, ty = List.assoc x !Normalize.global_arrays in
          let offset = Id.genid "o" in
          match ty with
          | Type.Float ->
              Asm.Let
                ( (offset, Type.Int),
                  Asm.Set addr,
                  Asm.Ans (Asm.StDF (z, y, Asm.V offset)) )
          | Type.Int ->
              Asm.Let
                ( (offset, Type.Int),
                  Asm.Set addr,
                  Asm.Ans (Asm.St (z, y, Asm.V offset)) )
          | _ -> assert false
      else if List.mem_assoc z !Normalize.global_arrays then
        let addr, ty = List.assoc z !Normalize.global_arrays in
        let z' = Id.genid "g" in
        Asm.Let
          ( (z', Type.Int),
            Asm.Set addr,
            match env_find x env with
            | Type.Array Type.Unit -> Asm.Ans Asm.Nop
            | Type.Array Type.Float -> Asm.Ans (Asm.StDF (z', x, Asm.V y))
            | Type.Array _ -> Asm.Ans (Asm.St (z', x, Asm.V y))
            | _ -> assert false )
      else
        match env_find x env with
        | Type.Array Type.Unit -> Asm.Ans Asm.Nop
        | Type.Array Type.Float -> Asm.Ans (Asm.StDF (z, x, Asm.V y))
        | Type.Array _ -> Asm.Ans (Asm.St (z, x, Asm.V y))
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
      (1, g (env_add x t (add_list yts (add_list zts []))) e)
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

let rec simm_g env = function
  | Asm.Ans exp -> Asm.Ans (simm_g' env exp)
  | Asm.Let ((x, t), Asm.Set i, e) when -32768 <= i && i < 32768 ->
      let e' = simm_g (env_add x i env) e in
      if List.mem x (Asm.fv e') then Asm.Let ((x, t), Asm.Set i, e') else e'
  | Asm.Let (xt, Asm.SLL (y, Asm.C i), e) when env_exists y env ->
      simm_g env (Asm.Let (xt, Asm.Set (env_find y env lsl i), e))
  | Asm.Let (xt, exp, e) -> Asm.Let (xt, simm_g' env exp, simm_g env e)

and simm_g' env = function
  | Asm.Add (x, Asm.V y) when env_exists y env ->
      Asm.Add (x, Asm.C (env_find y env))
  | Asm.Add (x, Asm.V y) when env_exists x env ->
      Asm.Add (y, Asm.C (env_find x env))
  | Asm.Sub (x, Asm.V y) when env_exists y env ->
      Asm.Sub (x, Asm.C (env_find y env))
  | Asm.SLL (x, Asm.V y) when env_exists y env ->
      Asm.SLL (x, Asm.C (env_find y env))
  | Asm.Ld (x, Asm.V y) when env_exists y env ->
      Asm.Ld (x, Asm.C (env_find y env))
  | Asm.St (x, y, Asm.V z) when env_exists z env ->
      Asm.St (x, y, Asm.C (env_find z env))
  | Asm.LdDF (x, Asm.V y) when env_exists y env ->
      Asm.LdDF (x, Asm.C (env_find y env))
  | Asm.StDF (x, y, Asm.V z) when env_exists z env ->
      Asm.StDF (x, y, Asm.C (env_find z env))
  (* | Asm.IfEq (x, Asm.V y, e1, e2) when env_exists y env ->
         Asm.IfEq (x, Asm.C (env_find y env), simm_g env e1, simm_g env e2)
     | Asm.IfLE (x, Asm.V y, e1, e2) when env_exists y env ->
         Asm.IfLE (x, Asm.C (env_find y env), simm_g env e1, simm_g env e2)
     | Asm.IfGE (x, Asm.V y, e1, e2) when env_exists y env ->
         Asm.IfGE (x, Asm.C (env_find y env), simm_g env e1, simm_g env e2)
     | Asm.IfEq (x, Asm.V y, e1, e2) when env_exists x env ->
         Asm.IfEq (y, Asm.C (env_find x env), simm_g env e1, simm_g env e2)
     | Asm.IfLE (x, Asm.V y, e1, e2) when env_exists x env ->
         Asm.IfGE (y, Asm.C (env_find x env), simm_g env e1, simm_g env e2)
     | Asm.IfGE (x, Asm.V y, e1, e2) when env_exists x env ->
         Asm.IfLE (y, Asm.C (env_find x env), simm_g env e1, simm_g env e2) *)
  | Asm.IfEq (x, y', e1, e2) -> Asm.IfEq (x, y', simm_g env e1, simm_g env e2)
  | Asm.IfLE (x, y', e1, e2) -> Asm.IfLE (x, y', simm_g env e1, simm_g env e2)
  | Asm.IfGE (x, y', e1, e2) -> Asm.IfGE (x, y', simm_g env e1, simm_g env e2)
  | Asm.IfFEq (x, y, e1, e2) -> Asm.IfFEq (x, y, simm_g env e1, simm_g env e2)
  | Asm.IfFLE (x, y, e1, e2) -> Asm.IfFLE (x, y, simm_g env e1, simm_g env e2)
  | e -> e

let simm_h
    { Asm.name = l; Asm.args = xs; Asm.fargs = ys; Asm.body = e; Asm.ret = t } =
  {
    Asm.name = l;
    Asm.args = xs;
    Asm.fargs = ys;
    Asm.body = simm_g [] e;
    Asm.ret = t;
  }

let simm (Asm.Prog (data, fundefs, e)) =
  Asm.Prog (data, List.map simm_h fundefs, simm_g [] e)

let print_i id = match id with Asm.V t -> t | Asm.C i -> string_of_int i

let rec print_exp e n =
  let ind = String.make n ' ' in
  ind
  ^
  match e with
  | Asm.Nop -> "nop"
  | Asm.Set i -> "set " ^ string_of_int i
  | Asm.SetF f -> "setf " ^ string_of_float f
  | Asm.SetL (L l) -> "setl " ^ l
  | Asm.Mov t -> "mov " ^ t
  | Asm.Neg t -> "neg " ^ t
  | Asm.Add (t, i) -> "add " ^ t ^ " " ^ print_i i
  | Asm.Sub (t, i) -> "sub " ^ t ^ " " ^ print_i i
  | Asm.SLL (t, i) -> "sll " ^ t ^ " " ^ print_i i
  | Asm.SRL (t, i) -> "srl " ^ t ^ " " ^ print_i i
  | Asm.Ld (t, i) -> "load " ^ t ^ " " ^ print_i i
  | Asm.St (t1, t2, i) -> "store " ^ t1 ^ " " ^ t2 ^ " " ^ print_i i
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
      "if " ^ id ^ " = " ^ print_i i ^ " then\n"
      ^ print_t t1 (n + 2)
      ^ "\n" ^ ind ^ "else\n"
      ^ print_t t2 (n + 2)
  | Asm.IfLE (id, i, t1, t2) ->
      "if " ^ id ^ " <= " ^ print_i i ^ " then\n"
      ^ print_t t1 (n + 2)
      ^ "\n" ^ ind ^ "else\n"
      ^ print_t t2 (n + 2)
  | Asm.IfGE (id, i, t1, t2) ->
      "if " ^ id ^ " >= " ^ print_i i ^ " then\n"
      ^ print_t t1 (n + 2)
      ^ "\n" ^ ind ^ "else\n"
      ^ print_t t2 (n + 2)
  | Asm.IfFEq (id1, id2, t1, t2) ->
      "if " ^ id1 ^ " =. " ^ id2 ^ " then\n"
      ^ print_t t1 (n + 2)
      ^ "\n" ^ ind ^ "else\n"
      ^ print_t t2 (n + 2)
  | Asm.IfFLE (id1, id2, t1, t2) ->
      "if " ^ id1 ^ " <=. " ^ id2 ^ " then\n"
      ^ print_t t1 (n + 2)
      ^ "\n" ^ ind ^ "else\n"
      ^ print_t t2 (n + 2)
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
  | Asm.Fiszero t -> "fiszero " ^ t
  | Asm.Fispos t -> "fispos " ^ t
  | Asm.Fisneg t -> "fisneg " ^ t
  | Asm.Fneg t -> "fneg " ^ t
  | Asm.Fabs t -> "fabs " ^ t
  | Asm.Fless (t1, t2) -> "fless " ^ t1 ^ " " ^ t2
  | Asm.Floor t -> "floor " ^ t
  | Asm.IntOfFloat t -> "int_of_float " ^ t
  | Asm.FloatOfInt t -> "float_of_int " ^ t
  | Asm.Sqrt t -> "sqrt " ^ t
  | Asm.Fsqr t -> "fsqr " ^ t

and print_t t n =
  let ind = String.make n ' ' in
  match t with
  | Asm.Ans e -> print_exp e n
  | Asm.Let ((id, typ), e, t') ->
      ind
      ^ (id ^ ": " ^ Type.print typ ^ " = " ^ print_exp e n ^ " \n")
      ^ print_t t' n

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
  ^ ")" ^ " -> " ^ Type.print ret ^ " =\n" ^ print_t body 0

let print t =
  let (Asm.Prog (fl, fundefs, e)) = t in
  List.fold_left (fun a b -> a ^ "\n\n[fun]\n" ^ print_fun b) "" fundefs
  ^ "\n[body]\n" ^ print_t e 0
