open Ds
open Asm

(* for register coalescing *)
(* [XXX] Callがあったら、そこから先は無意味というか逆効果なので追わない。
         そのために「Callがあったかどうか」を返り値の第1要素に含める。 *)
let rec target' src (dest, t) = function
  | Mov x when x = src && is_reg dest ->
      assert (t <> Type.Unit);
      assert (t <> Type.Float);
      (false, [ dest ])
  | FMovD x when x = src && is_reg dest ->
      assert (t = Type.Float);
      (false, [ dest ])
  | IfEq (_, _, e1, e2)
  | IfLE (_, _, e1, e2)
  | IfGE (_, _, e1, e2)
  | IfFEq (_, _, e1, e2)
  | IfFLE (_, _, e1, e2) ->
      let c1, rs1 = target src (dest, t) e1 in
      let c2, rs2 = target src (dest, t) e2 in
      (c1 && c2, rs1 @ rs2)
  | CallCls (x, ys, zs) ->
      ( true,
        target_args src regs 0 ys @ target_args src fregs 0 zs
        @ if x = src then [ reg_cl ] else [] )
  | CallDir (_, ys, zs) ->
      (true, target_args src regs 0 ys @ target_args src fregs 0 zs)
  | _ -> (false, [])

and target src dest = function
  (* register targeting *)
  | Ans exp -> target' src dest exp
  | Let (xt, exp, e) ->
      let c1, rs1 = target' src xt exp in
      if c1 then (true, rs1)
      else
        let c2, rs2 = target src dest e in
        (c2, rs1 @ rs2)

and target_args src all n = function
  (* auxiliary function for Call *)
  | [] -> []
  | y :: ys when src = y -> all.(n) :: target_args src all (n + 1) ys
  | _ :: ys -> target_args src all (n + 1) ys

type alloc_result =
  (* allocにおいてspillingがあったかどうかを表すデータ型 *)
  | Alloc of Id.t (* allocated register *)
  | Spill of Id.t
(* spilled variable *)

let rec alloc dest cont regenv x t =
  (* allocate a register or spill a variable *)
  (* assert (not (env_exists x regenv)); *)
  let all =
    match t with
    | Type.Unit -> [ "a21" ]
    | Type.Float -> allfregs
    | _ -> allregs
  in
  if all = [ "a21" ] then Alloc "a21"
  else if (* [XX] ad hoc optimization *)
          is_reg x then Alloc x
  else
    let free = fv cont in
    try
      let c, prefer = target x dest cont in
      let live =
        (* 生きているレジスタ *)
        List.fold_left
          (fun live y ->
            if is_reg y then set_add y live
            else try set_add (env_find y regenv) live with Not_found -> live)
          [] free
      in
      let r =
        (* そうでないレジスタを探す *)
        List.find (fun r -> not (set_exist r live)) (prefer @ all)
      in
      (* Format.eprintf "allocated %s to %s@." x r; *)
      Alloc r
    with Not_found ->
      Format.eprintf "register allocation failed for %s@." x;
      let y =
        (* 型の合うレジスタ変数を探す *)
        List.find
          (fun y ->
            (not (is_reg y))
            && try List.mem (env_find y regenv) all with Not_found -> false)
          (List.rev free)
      in
      Format.eprintf "spilling %s from %s@." y (env_find y regenv);
      Spill y

(* auxiliary function for g and g'_and_restore *)
let add x r regenv =
  if is_reg x then (
    assert (x = r);
    regenv)
  else env_add x r regenv

(* auxiliary functions for g' *)
exception NoReg of Id.t * Type.t

let find x t regenv =
  if is_reg x then x
  else try env_find x regenv with Not_found -> raise (NoReg (x, t))

let find' x' regenv = match x' with V x -> V (find x Type.Int regenv) | c -> c

let rec g dest cont regenv = function
  (* 命令列のレジスタ割り当て *)
  | Ans exp -> g'_and_restore dest cont regenv exp
  | Let (((x, t) as xt), exp, e) -> (
      (* assert (not (env_exists x regenv)); *)
      let cont' = concat e dest cont in
      let e1', regenv1 = g'_and_restore xt cont' regenv exp in
      match alloc dest cont' regenv1 x t with
      | Spill y ->
          let r = env_find y regenv1 in
          let e2', regenv2 = g dest cont (add x r (env_remove y regenv1)) e in
          let save = try Save (env_find y regenv, y) with Not_found -> Nop in
          (seq (save, concat e1' (r, t) e2'), regenv2)
      | Alloc r ->
          let e2', regenv2 = g dest cont (add x r regenv1) e in
          (concat e1' (r, t) e2', regenv2))

and g'_and_restore dest cont regenv exp =
  (* 使用される変数をスタックからレジスタへRestore *)
  try g' dest cont regenv exp
  with NoReg (x, t) ->
    (* Format.eprintf "restoring %s@." x; *)
    g dest cont regenv (Let ((x, t), Restore x, Ans exp))

and g' dest cont regenv = function
  (* 各命令のレジスタ割り当て *)
  | (Nop | Set _ | SetF _ | SetL _ | Comment _ | Restore _) as exp ->
      (Ans exp, regenv)
  | Mov x -> (Ans (Mov (find x Type.Int regenv)), regenv)
  | Neg x -> (Ans (Neg (find x Type.Int regenv)), regenv)
  | Add (x, y') -> (Ans (Add (find x Type.Int regenv, find' y' regenv)), regenv)
  | Sub (x, y') -> (Ans (Sub (find x Type.Int regenv, find' y' regenv)), regenv)
  | SLL (x, y') -> (Ans (SLL (find x Type.Int regenv, find' y' regenv)), regenv)
  | SRL (x, y') -> (Ans (SRL (find x Type.Int regenv, find' y' regenv)), regenv)
  | Ld (x, y') -> (Ans (Ld (find x Type.Int regenv, find' y' regenv)), regenv)
  | St (x, y, z') ->
      ( Ans
          (St (find x Type.Int regenv, find y Type.Int regenv, find' z' regenv)),
        regenv )
  | FMovD x -> (Ans (FMovD (find x Type.Float regenv)), regenv)
  | FNegD x -> (Ans (FNegD (find x Type.Float regenv)), regenv)
  | FAddD (x, y) ->
      (Ans (FAddD (find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FSubD (x, y) ->
      (Ans (FSubD (find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FMulD (x, y) ->
      (Ans (FMulD (find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FDivD (x, y) ->
      (Ans (FDivD (find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | LdDF (x, y') ->
      (Ans (LdDF (find x Type.Int regenv, find' y' regenv)), regenv)
  | StDF (x, y, z') ->
      ( Ans
          (StDF
             (find x Type.Float regenv, find y Type.Int regenv, find' z' regenv)),
        regenv )
  | IfEq (x, y', e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          IfEq (find x Type.Int regenv, find' y' regenv, e1', e2'))
        e1 e2
  | IfLE (x, y', e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          IfLE (find x Type.Int regenv, find' y' regenv, e1', e2'))
        e1 e2
  | IfGE (x, y', e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          IfGE (find x Type.Int regenv, find' y' regenv, e1', e2'))
        e1 e2
  | IfFEq (x, y, e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          IfFEq (find x Type.Float regenv, find y Type.Float regenv, e1', e2'))
        e1 e2
  | IfFLE (x, y, e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          IfFLE (find x Type.Float regenv, find y Type.Float regenv, e1', e2'))
        e1 e2
  | CallCls (x, ys, zs) as exp ->
      g'_call dest cont regenv exp
        (fun ys zs -> CallCls (find x Type.Int regenv, ys, zs))
        ys zs
  | CallDir (l, ys, zs) as exp ->
      g'_call dest cont regenv exp (fun ys zs -> CallDir (l, ys, zs)) ys zs
  | Save (x, y) -> assert false
  | Fiszero x -> (Ans (Fiszero (find x Type.Float regenv)), regenv)
  | Fispos x -> (Ans (Fispos (find x Type.Float regenv)), regenv)
  | Fisneg x -> (Ans (Fisneg (find x Type.Float regenv)), regenv)
  | Fneg x -> (Ans (Fneg (find x Type.Float regenv)), regenv)
  | Fabs x -> (Ans (Fabs (find x Type.Float regenv)), regenv)
  | Fless (x, y) ->
      (Ans (Fless (find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | Floor x -> (Ans (Floor (find x Type.Float regenv)), regenv)
  | IntOfFloat x -> (Ans (IntOfFloat (find x Type.Float regenv)), regenv)
  | FloatOfInt x -> (Ans (FloatOfInt (find x Type.Int regenv)), regenv)
  | Sqrt x -> (Ans (Sqrt (find x Type.Float regenv)), regenv)
  | Fsqr x -> (Ans (Fsqr (find x Type.Float regenv)), regenv)

and g'_if dest cont regenv exp constr e1 e2 =
  (* ifのレジスタ割り当て *)
  let e1', regenv1 = g dest cont regenv e1 in
  let e2', regenv2 = g dest cont regenv e2 in
  let regenv' =
    (* 両方に共通のレジスタ変数だけ利用 *)
    List.fold_left
      (fun regenv' x ->
        try
          if is_reg x then regenv'
          else
            let r1 = env_find x regenv1 in
            let r2 = env_find x regenv2 in
            if r1 <> r2 then regenv' else env_add x r1 regenv'
        with Not_found -> regenv')
      [] (fv cont)
  in
  ( List.fold_left
      (fun e x ->
        if x = fst dest || (not (env_exists x regenv)) || env_exists x regenv'
        then e
        else seq (Save (env_find x regenv, x), e))
      (* そうでない変数は分岐直前にセーブ *)
      (Ans (constr e1' e2'))
      (fv cont),
    regenv' )

and g'_call dest cont regenv exp constr ys zs =
  (* 関数呼び出しのレジスタ割り当て *)
  ( List.fold_left
      (fun e x ->
        if x = fst dest || not (env_exists x regenv) then e
        else seq (Save (env_find x regenv, x), e))
      (Ans
         (constr
            (List.map (fun y -> find y Type.Int regenv) ys)
            (List.map (fun z -> find z Type.Float regenv) zs)))
      (fv cont),
    [] )

let h { name = Id.L x; args = ys; fargs = zs; body = e; ret = t } =
  (* 関数のレジスタ割り当て *)
  let regenv = env_add x reg_cl [] in
  let i, arg_regs, regenv =
    List.fold_left
      (fun (i, arg_regs, regenv) y ->
        let r = regs.(i) in
        ( i + 1,
          arg_regs @ [ r ],
          (assert (not (is_reg y));
           env_add y r regenv) ))
      (0, [], regenv) ys
  in
  let d, farg_regs, regenv =
    List.fold_left
      (fun (d, farg_regs, regenv) z ->
        let fr = fregs.(d) in
        ( d + 1,
          farg_regs @ [ fr ],
          (assert (not (is_reg z));
           env_add z fr regenv) ))
      (0, [], regenv) zs
  in
  let a =
    match t with
    | Type.Unit -> Id.gentmp Type.Unit
    | Type.Float -> fregs.(0)
    | _ -> regs.(0)
  in
  let e', regenv' = g (a, t) (Ans (Mov a)) regenv e in
  { name = Id.L x; args = arg_regs; fargs = farg_regs; body = e'; ret = t }

let f (Prog (data, fundefs, e)) =
  (* プログラム全体のレジスタ割り当て *)
  Format.eprintf
    "register allocation: may take some time (up to a few minutes, depending \
     on the size of functions)@.";
  let fundefs' = List.map h fundefs in
  let e', regenv' = g (Id.gentmp Type.Unit, Type.Unit) (Ans Nop) [] e in
  Prog (data, fundefs', e')
