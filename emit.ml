open Ds
open Asm

type reg = string

type label = string

exception Selected

type t = exp list

and exp =
  | Nop
  | Li of reg * int
  | Fli of reg * float
  | La of reg * label
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Addi of reg * reg * int
  | Sll of reg * reg * reg
  | Srl of reg * reg * reg
  | Slli of reg * reg * int
  | Srli of reg * reg * int
  | Fadd of reg * reg * reg
  | Fsub of reg * reg * reg
  | Fmul of reg * reg * reg
  | Fdiv of reg * reg * reg
  | Fmv of reg * reg
  | Fmvxw of reg * reg
  | Fmvwx of reg * reg
  | Fneg of reg * reg
  | Lw of reg * int * reg
  | Flw of reg * int * reg
  | Sw of reg * int * reg
  | Fsw of reg * int * reg
  | Beq of reg * reg * label
  | Bne of reg * reg * label
  | Blt of reg * reg * label
  | Feq of reg * reg * reg
  | Fle of reg * reg * reg
  | Flt of reg * reg * reg
  | IntOfFloat of reg * reg
  | FloatOfInt of reg * reg
  | Fabs of reg * reg
  | Floor of reg * reg
  | Sqrt of reg * reg
  | Jump of label
  | Jalr of reg * reg * int
  | Jal of reg * reg
  | Label of label

let insns = ref []

let stackset = ref [] (* すでにSaveされた変数の集合 *)

let stackmap = ref []
(* Saveされた変数の、スタックにおける位置 *)

let save x =
  stackset := set_add x !stackset;
  if not (List.mem x !stackmap) then stackmap := !stackmap @ [ x ]

let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs)
  in
  loc !stackmap

let offset x = -1 * List.hd (locate x)

let stacksize () = align (List.length !stackmap * 1)

let pp_id_or_imm = function V x -> x | C i -> string_of_int i

(* 関数呼び出しのために引数を並べ替える(register shuffling) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] ->
      (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y)
      :: shuffle sw
           (List.map (function y', z when y = y' -> (sw, z) | yz -> yz) xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t
(* 末尾かどうかを表すデータ型 *)

let rec g = function
  (* 命令列のアセンブリ生成 *)
  | dest, Ans exp -> g' (dest, exp)
  | dest, Let ((x, t), exp, e) ->
      g' (NonTail x, exp);
      g (dest, e)

and g' = function
  (* 各命令のアセンブリ生成 *)
  (* 末尾でなかったら計算結果をdestにセット *)
  | NonTail _, Nop -> ()
  | NonTail x, Set i -> insns := Li (x, i) :: !insns
  | NonTail x, SetF f -> insns := Fli (x, f) :: !insns
  | NonTail x, SetL (Id.L y) -> insns := La (x, y) :: !insns
  | NonTail x, Mov y when x = y -> ()
  | NonTail x, Mov y -> insns := Add (x, y, "zero") :: !insns
  | NonTail x, Neg y ->
      if List.mem x allregs && List.mem y allregs then
        insns := Sub (x, "zero", y) :: !insns
      else if List.mem x allregs then (
        insns := Fmvxw (x, y) :: !insns;
        insns := Sub (x, "zero", x) :: !insns)
      else if List.mem y allregs then (
        insns := Fmvwx (x, y) :: !insns;
        insns := Fneg (x, x) :: !insns)
      else insns := Fneg (x, y) :: !insns
  | NonTail x, Add (y, z') -> (
      match z' with
      | V id -> insns := Add (x, y, id) :: !insns
      | C i -> insns := Addi (x, y, i) :: !insns)
  | NonTail x, Sub (y, z') -> (
      match z' with
      | V id -> insns := Sub (x, y, id) :: !insns
      | C i -> insns := Addi (x, y, -1 * i) :: !insns)
  | NonTail x, SLL (y, z') -> (
      match z' with
      | V id -> insns := Sll (x, y, id) :: !insns
      | C i -> insns := Slli (x, y, i) :: !insns)
  | NonTail x, SRL (y, z') -> (
      match z' with
      | V id -> insns := Srl (x, y, id) :: !insns
      | C i -> insns := Srli (x, y, i) :: !insns)
  | NonTail x, Ld (y, z') -> (
      match z' with
      | V id ->
          insns := Add ("a22", id, y) :: !insns;
          if List.mem x allregs then insns := Lw (x, 0, "a22") :: !insns
          else insns := Flw (x, 0, "a22") :: !insns
      | C i -> insns := Lw (x, i, y) :: !insns)
  | NonTail _, St (x, y, z') -> (
      match z' with
      | V id ->
          insns := Add ("a22", id, y) :: !insns;
          if List.mem x allregs then insns := Sw (x, 0, "a22") :: !insns
          else insns := Fsw (x, 0, "a22") :: !insns
      | C i -> insns := Sw (x, i, y) :: !insns)
  | NonTail x, FMovD y when x = y -> ()
  | NonTail x, FMovD y when List.mem x allregs ->
      insns := Fmvxw (x, y) :: !insns
  | NonTail x, FMovD y -> insns := Fadd (x, y, "fzero") :: !insns
  | NonTail x, FNegD y -> insns := Fneg (x, y) :: !insns
  | NonTail x, FAddD (y, z) -> insns := Fadd (x, y, z) :: !insns
  | NonTail x, FSubD (y, z) -> insns := Fsub (x, y, z) :: !insns
  | NonTail x, FMulD (y, z) -> insns := Fmul (x, y, z) :: !insns
  | NonTail x, FDivD (y, z) -> insns := Fdiv (x, y, z) :: !insns
  | NonTail x, Fiszero y -> insns := Feq (x, y, "fzero") :: !insns
  | NonTail x, Fispos y -> insns := Flt (x, "fzero", y) :: !insns
  | NonTail x, Fisneg y -> insns := Flt (x, y, "fzero") :: !insns
  | NonTail x, Fneg y -> insns := Fneg (x, y) :: !insns
  | NonTail x, Fabs y -> insns := Fabs (x, y) :: !insns
  | NonTail x, Fless (y, z) -> insns := Flt (x, y, z) :: !insns
  | NonTail x, Floor y -> insns := Floor (x, y) :: !insns
  | NonTail x, IntOfFloat y -> insns := IntOfFloat (x, y) :: !insns
  | NonTail x, FloatOfInt y -> insns := FloatOfInt (x, y) :: !insns
  | NonTail x, Sqrt y -> insns := Sqrt (x, y) :: !insns
  | NonTail x, Fsqr y -> insns := Fmul (x, y, y) :: !insns
  | NonTail x, LdDF (y, z') -> (
      match z' with
      | V id ->
          insns := Add ("a22", id, y) :: !insns;
          if List.mem x allregs then insns := Lw (x, 0, "a22") :: !insns
          else insns := Flw (x, 0, "a22") :: !insns
      | C i -> insns := Flw (x, i, y) :: !insns)
  | NonTail _, StDF (x, y, z') -> (
      match z' with
      | V id ->
          insns := Add ("a22", id, y) :: !insns;
          if List.mem x allregs then insns := Sw (x, 0, "a22") :: !insns
          else insns := Fsw (x, 0, "a22") :: !insns
      | C i -> insns := Fsw (x, i, y) :: !insns)
  | NonTail _, Comment s -> ()
  (* 退避の仮想命令の実装 *)
  | NonTail _, Save (x, y)
    when List.mem x allregs && not (set_exist y !stackset) ->
      if y = "zero" || y = "fzero" || env_exists y !Sched.substitutes then ()
      else (
        save y;
        insns := Sw (x, offset y, reg_sp) :: !insns)
  | NonTail _, Save (x, y)
    when List.mem x allfregs && not (set_exist y !stackset) ->
      if y = "zero" || y = "fzero" || env_exists y !Sched.substitutes then ()
      else (
        save y;
        insns := Fsw (x, offset y, reg_sp) :: !insns)
  | NonTail _, Save (x, y) ->
      assert (set_exist y !stackset);
      ()
  (* 復帰の仮想命令の実装 *)
  | NonTail x, Restore y when List.mem x allregs ->
      if env_exists y !Sched.substitutes then
        let tmp = env_find y !Sched.substitutes in
        if tmp = "zero" then insns := Li (x, 0) :: !insns
        else insns := Fli (x, 0.0) :: !insns
      else if y = "zero" then insns := Li (x, 0) :: !insns
      else if y = "fzero" then insns := Fli (x, 0.0) :: !insns
      else insns := Lw (x, offset y, reg_sp) :: !insns
  | NonTail x, Restore y ->
      assert (List.mem x allfregs);
      if env_exists y !Sched.substitutes then
        let tmp = env_find y !Sched.substitutes in
        if tmp = "zero" then insns := Li (x, 0) :: !insns
        else insns := Fli (x, 0.0) :: !insns
      else if y = "zero" then insns := Li (x, 0) :: !insns
      else if y = "fzero" then insns := Fli (x, 0.0) :: !insns
      else insns := Flw (x, offset y, reg_sp) :: !insns
  (* 末尾だったら計算結果を第一レジスタにセットしてret *)
  | Tail, ((Nop | St _ | StDF _ | Comment _ | Save _) as exp) ->
      g' (NonTail (Id.gentmp Type.Unit), exp);
      insns := Jalr ("zero", "ra", 0) :: !insns
  | ( Tail,
      (( Set _ | SetL _ | Mov _ | Add _ | Sub _ | SLL _ | SRL _ | Ld _
       | Fiszero _ | Fispos _ | Fisneg _ | Fless _ | IntOfFloat _ ) as exp) ) ->
      g' (NonTail regs.(0), exp);
      insns := Jalr ("zero", "ra", 0) :: !insns
  | Tail, (Neg x as exp) ->
      if List.mem x allregs then g' (NonTail regs.(0), exp)
      else g' (NonTail fregs.(0), exp);
      insns := Jalr ("zero", "ra", 0) :: !insns
  | ( Tail,
      (( SetF _ | FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _
       | LdDF _ | Fneg _ | Fabs _ | Floor _ | FloatOfInt _ | Sqrt _ | Fsqr _ )
      as exp) ) ->
      g' (NonTail fregs.(0), exp);
      insns := Jalr ("zero", "ra", 0) :: !insns
  | Tail, (Restore x as exp) ->
      (match locate x with
      | [ i ] -> g' (NonTail regs.(0), exp)
      | [ i; j ] when i + 1 = j -> g' (NonTail fregs.(0), exp)
      | _ -> assert false);
      insns := Jalr ("zero", "ra", 0) :: !insns
  | Tail, IfEq (x, y', e1, e2) ->
      let b_else = Id.genid "be_else" in
      insns := Bne (x, pp_id_or_imm y', b_else) :: !insns;
      let stackset_back = !stackset in
      g (Tail, e1);
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (Tail, e2)
  | Tail, IfLE (x, y', e1, e2) ->
      let b_else = Id.genid "ble_else" in
      insns := Blt (pp_id_or_imm y', x, b_else) :: !insns;
      let stackset_back = !stackset in
      g (Tail, e1);
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (Tail, e2)
  | Tail, IfGE (x, y', e1, e2) ->
      let b_else = Id.genid "bge_else" in
      insns := Blt (x, pp_id_or_imm y', b_else) :: !insns;
      let stackset_back = !stackset in
      g (Tail, e1);
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (Tail, e2)
  | Tail, IfFEq (x, y, e1, e2) ->
      let b_else = Id.genid "fbe_else" in
      insns := Feq ("a20", x, y) :: !insns;
      insns := Beq ("a20", "zero", b_else) :: !insns;
      let stackset_back = !stackset in
      g (Tail, e1);
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (Tail, e2)
  | Tail, IfFLE (x, y, e1, e2) ->
      let b_else = Id.genid "fble_else" in
      insns := Fle ("a20", x, y) :: !insns;
      insns := Beq ("a20", "zero", b_else) :: !insns;
      let stackset_back = !stackset in
      g (Tail, e1);
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (Tail, e2)
  | NonTail z, IfEq (x, y', e1, e2) ->
      let b_else = Id.genid "be_else" in
      let b_cont = Id.genid "be_cont" in
      insns := Bne (x, pp_id_or_imm y', b_else) :: !insns;
      let stackset_back = !stackset in
      g (NonTail z, e1);
      let stackset1 = !stackset in
      insns := Jump b_cont :: !insns;
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (NonTail z, e2);
      insns := Label b_cont :: !insns;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  | NonTail z, IfLE (x, y', e1, e2) ->
      let b_else = Id.genid "ble_else" in
      let b_cont = Id.genid "ble_cont" in
      insns := Blt (pp_id_or_imm y', x, b_else) :: !insns;
      let stackset_back = !stackset in
      g (NonTail z, e1);
      let stackset1 = !stackset in
      insns := Jump b_cont :: !insns;
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (NonTail z, e2);
      insns := Label b_cont :: !insns;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  | NonTail z, IfGE (x, y', e1, e2) ->
      let b_else = Id.genid "bge_else" in
      let b_cont = Id.genid "bge_cont" in
      insns := Blt (x, pp_id_or_imm y', b_else) :: !insns;
      let stackset_back = !stackset in
      g (NonTail z, e1);
      let stackset1 = !stackset in
      insns := Jump b_cont :: !insns;
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (NonTail z, e2);
      insns := Label b_cont :: !insns;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  | NonTail z, IfFEq (x, y, e1, e2) ->
      let b_else = Id.genid "fbe_else" in
      let b_cont = Id.genid "fbe_cont" in
      insns := Feq ("a20", x, y) :: !insns;
      insns := Beq ("a20", "zero", b_else) :: !insns;
      let stackset_back = !stackset in
      g (NonTail z, e1);
      let stackset1 = !stackset in
      insns := Jump b_cont :: !insns;
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (NonTail z, e2);
      insns := Label b_cont :: !insns;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  | NonTail z, IfFLE (x, y, e1, e2) ->
      let b_else = Id.genid "fble_else" in
      let b_cont = Id.genid "fble_cont" in
      insns := Fle ("a20", x, y) :: !insns;
      insns := Beq ("a20", "zero", b_else) :: !insns;
      let stackset_back = !stackset in
      g (NonTail z, e1);
      let stackset1 = !stackset in
      insns := Jump b_cont :: !insns;
      insns := Label b_else :: !insns;
      stackset := stackset_back;
      g (NonTail z, e2);
      insns := Label b_cont :: !insns;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  (* 関数呼び出しの仮想命令の実装 *)
  | Tail, CallCls (x, ys, zs) ->
      (* 末尾呼び出し *)
      g'_args [ (x, reg_cl) ] ys zs;
      insns := Lw (reg_sw, 0, reg_cl) :: !insns;
      insns := Jalr ("zero", reg_sw, 0) :: !insns
  | Tail, CallDir (Id.L x, ys, zs) ->
      (* 末尾呼び出し *)
      g'_args [] ys zs;
      insns := Jump x :: !insns
  | NonTail a, CallCls (x, ys, zs) ->
      g'_args [ (x, reg_cl) ] ys zs;
      let ss = stacksize () in
      insns := Sw (reg_ra, -ss, reg_sp) :: !insns;
      insns := Lw (reg_sw, 0, reg_cl) :: !insns;
      insns := Addi (reg_sp, reg_sp, (-1 * ss) - 1) :: !insns;
      insns := Jalr ("ra", reg_sw, 0) :: !insns;
      insns := Addi (reg_sp, reg_sp, ss + 1) :: !insns;
      insns := Lw (reg_ra, -ss, reg_sp) :: !insns;
      if List.mem a allregs && a <> regs.(0) then
        insns := Add (a, regs.(0), "zero") :: !insns
      else if List.mem a allfregs && a <> fregs.(0) then
        insns := Fadd (a, fregs.(0), "fzero") :: !insns
  | NonTail a, CallDir (Id.L x, ys, zs) ->
      g'_args [] ys zs;
      let ss = stacksize () in
      insns := Sw (reg_ra, -ss, reg_sp) :: !insns;
      insns := Addi (reg_sp, reg_sp, (-1 * ss) - 1) :: !insns;
      insns := Jal ("ra", x) :: !insns;
      insns := Addi (reg_sp, reg_sp, ss + 1) :: !insns;
      insns := Lw (reg_ra, -ss, reg_sp) :: !insns;
      if List.mem a allregs && a <> regs.(0) then
        insns := Add (a, regs.(0), "zero") :: !insns
      else if List.mem a allfregs && a <> fregs.(0) then
        insns := Fadd (a, fregs.(0), "fzero") :: !insns

and g'_args x_reg_cl ys zs =
  let i, yrs =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl) ys
  in
  List.iter
    (fun (y, r) -> insns := Add (r, y, "zero") :: !insns)
    (shuffle reg_sw yrs);
  let d, zfrs =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, []) zs
  in
  List.iter
    (fun (z, fr) -> insns := Fmv (fr, z) :: !insns)
    (shuffle reg_fsw zfrs)

let h { name = Id.L x; args = _; fargs = _; body = e; ret = _ } =
  insns := Label x :: !insns;
  stackset := [];
  stackmap := [];
  g (Tail, e)

let rec f (Prog (data, fundefs, e)) =
  List.iter (fun fundef -> h fundef) fundefs;
  insns := Label "min_caml_start" :: !insns;
  insns := Addi ("sp", "sp", -28) :: !insns;
  (* from gcc; why 112? -> changed to 28 (for no reason)*)
  stackset := [];
  stackmap := [];
  g (NonTail "a0", e);
  insns := Jalr ("zero", "ra", 0) :: !insns;
  List.rev !insns

let rd a =
  match a with
  | Li (x, _) -> [ x ]
  | Fli (x, _) -> [ x ]
  | La (x, _) -> [ x ]
  | Add (x, _, _) -> [ x ]
  | Sub (x, _, _) -> [ x ]
  | Addi (x, _, _) -> [ x ]
  | Sll (x, _, _) -> [ x ]
  | Srl (x, _, _) -> [ x ]
  | Slli (x, _, _) -> [ x ]
  | Srli (x, _, _) -> [ x ]
  | Fadd (x, _, _) -> [ x ]
  | Fsub (x, _, _) -> [ x ]
  | Fmul (x, _, _) -> [ x ]
  | Fdiv (x, _, _) -> [ x ]
  | Fmv (x, _) -> [ x ]
  | Fmvxw (x, _) -> [ x ]
  | Fmvwx (x, _) -> [ x ]
  | Fneg (x, _) -> [ x ]
  | Lw (x, _, _) -> [ x ]
  | Flw (x, _, _) -> [ x ]
  | Feq (x, _, _) -> [ x ]
  | Fle (x, _, _) -> [ x ]
  | Flt (x, _, _) -> [ x ]
  | IntOfFloat (x, _) -> [ x ]
  | FloatOfInt (x, _) -> [ x ]
  | Fabs (x, _) -> [ x ]
  | Floor (x, _) -> [ x ]
  | _ -> []

let rs a =
  match a with
  | Add (_, y, z) -> [ y; z ]
  | Sub (_, y, z) -> [ y; z ]
  | Addi (_, y, _) -> [ y ]
  | Sll (_, y, z) -> [ y; z ]
  | Srl (_, y, z) -> [ y; z ]
  | Slli (_, y, _) -> [ y ]
  | Srli (_, y, _) -> [ y ]
  | Fadd (_, y, z) -> [ y; z ]
  | Fsub (_, y, z) -> [ y; z ]
  | Fmul (_, y, z) -> [ y; z ]
  | Fdiv (_, y, z) -> [ y; z ]
  | Fmv (_, y) -> [ y ]
  | Fmvxw (_, y) -> [ y ]
  | Fmvwx (_, y) -> [ y ]
  | Fneg (_, y) -> [ y ]
  | Lw (_, _, z) -> [ z ]
  | Flw (_, _, z) -> [ z ]
  | Sw (x, _, z) -> [ x; z ]
  | Fsw (x, _, z) -> [ x; z ]
  | Feq (_, y, z) -> [ y; z ]
  | Fle (_, y, z) -> [ y; z ]
  | Flt (_, y, z) -> [ y; z ]
  | IntOfFloat (_, y) -> [ y ]
  | FloatOfInt (_, y) -> [ y ]
  | Fabs (_, y) -> [ y ]
  | Floor (_, y) -> [ y ]
  | _ -> []

let rec intersect l1 l2 =
  match l1 with
  | [] -> false
  | x :: _ -> (
      match l2 with
      | [] -> false
      | [ y ] -> if x = y then true else false
      | y :: z -> if x = y || x = List.hd z then true else false)

let is_depend a b =
  match (a, b) with
  | Sw (_, _, _), Sw (_, _, _)
  | Fsw (_, _, _), Fsw (_, _, _)
  | Sw (_, _, _), Fsw (_, _, _)
  | Fsw (_, _, _), Sw (_, _, _) ->
      true
  | Nop, _ | _, Nop -> false
  | _, _ ->
      let rd_a = rd a in
      let rd_b = rd b in
      let rs_a = rs a in
      let rs_b = rs b in
      intersect rd_a rs_b (* RAW *)
      || intersect rd_b rs_a (* WAR *)
      || intersect rd_a rd_b
(* WAW *)

let is_depend_raw a b =
  match (a, b) with
  (* we suppose here that no two adjacent
     store instructions are about the same address *)
  (* | Sw (_, _, _), Sw (_, _, _)
     | Fsw (_, _, _), Fsw (_, _, _)
     | Sw (_, _, _), Fsw (_, _, _)
     | Fsw (_, _, _), Sw (_, _, _) ->
         true *)
  | Nop, _ | _, Nop -> false
  | _, _ ->
      let rd_a = rd a in
      let rs_b = rs b in
      intersect rd_a rs_b

let rec n_in graph i =
  match graph with
  | (x, y) :: rest -> if i = y then 1 + n_in rest i else n_in rest i
  | [] -> 0

let rec del graph i =
  match graph with
  | (x, y) :: rest ->
      if i = x || i = y then del rest i else (x, y) :: del rest i
  | [] -> []

(* let list_sched blk =
   let graph = ref [] in
   (* (from, to) *)
   let n = List.length blk in
   for i = 0 to n - 1 do
     for j = 0 to n - 1 do
       if i < j then
         let a = List.nth blk i in
         let b = List.nth blk j in
         if is_depend a b then graph := !graph @ [ (i, j) ] else ()
       else ()
     done
   done;
   let res = ref [] in
   let isused = Array.make n false in
   let b = ref false in
   while List.length !res < n do
     (try
        for i = 0 to n - 1 do
          match List.nth blk i with
          | (Lw (_, _, _) | Flw (_, _, _))
            when n_in !graph i = 0 && isused.(i) = false && !b = false ->
              isused.(i) <- true;
              b := true;
              graph := del !graph i;
              res := !res @ [ List.nth blk i ];
              raise Selected
          | _ -> ()
        done
      with Selected -> ());
     (try
        for i = 0 to n - 1 do
          match List.nth blk i with
          | Lw (_, _, _) | Flw (_, _, _) -> ()
          | _ when n_in !graph i = 0 && isused.(i) = false && !b = false ->
              isused.(i) <- true;
              b := true;
              graph := del !graph i;
              res := !res @ [ List.nth blk i ];
              raise Selected
          | _ -> ()
        done
      with Selected -> ());
     if !b = false then Format.eprintf "not proceed@.";
     b := false
   done;
   !res *)

let rec list_sched blk =
  let rec swap blk =
    match blk with
    | [] -> []
    | [ z ] -> [ z ]
    | a :: rest -> (
        let b = List.hd rest in
        match b with
        | Lw _ | Flw _ ->
            if is_depend a b then a :: swap rest
            else b :: swap (a :: List.tl rest)
        | _ -> a :: swap rest)
  in
  let rec iter b n =
    if n = 0 then b
    else
      let b' = swap b in
      if b = b' then b else iter b' (n - 1)
  in
  iter blk 10000

let rec sched insns cur_blk =
  match insns with
  | [] -> cur_blk
  | hd :: rest -> (
      match hd with
      | Beq (_, _, _)
      | Bne (_, _, _)
      | Blt (_, _, _)
      | Jump _
      | Jalr (_, _, _)
      | Jal (_, _)
      | Label _ ->
          let blk = list_sched cur_blk in
          blk @ [ hd ] @ sched rest []
      | _ as insn ->
          let blk = cur_blk @ [ insn ] in
          sched rest blk)

let rec optimize insns =
  let rec replace = function
    | [] -> []
    | [ z ] -> [ z ]
    | cur :: rest -> (
        let default _ = cur :: replace rest in
        try
          match cur with
          | Add (x, y, z) when z = "zero" -> (
              match List.hd rest with
              | Add (x', y', z') when z' = "zero" ->
                  if x = y' then Add (x', y, "zero") :: replace (List.tl rest)
                  else default ()
              | _ -> default ())
          | Addi (x, y, i) -> (
              match List.hd rest with
              | Addi (x', y', i') ->
                  if x = y && x' = y' && x = x' && y = y' && i = -i' then
                    replace (List.tl rest)
                  else default ()
              | _ -> default ())
          | Lw (lx, li, ly) -> (
              match List.hd rest with
              | Sw (sx, si, sy) ->
                  if lx = sx && li = si && ly = sy then
                    cur :: replace (List.tl rest)
                  else default ()
              | _ -> default ())
          | Flw (lx, li, ly) -> (
              match List.hd rest with
              | Fsw (sx, si, sy) ->
                  if lx = sx && li = si && ly = sy then
                    cur :: replace (List.tl rest)
                  else default ()
              | _ -> default ())
          | Sw (lx, li, ly) -> (
              match List.hd rest with
              | Lw (sx, si, sy) ->
                  if lx = sx && li = si && ly = sy then
                    cur :: replace (List.tl rest)
                  else default ()
              | _ -> default ())
          | Fsw (lx, li, ly) -> (
              match List.hd rest with
              | Flw (sx, si, sy) ->
                  if lx = sx && li = si && ly = sy then
                    cur :: replace (List.tl rest)
                  else default ()
              | _ -> default ())
          | e -> default ()
        with _ -> default ())
  in
  let rec iter n e =
    if n = 0 then e
    else
      let e' = replace e in
      if e = e' then e else iter (n - 1) e'
  in
  let insns = iter 1000 insns in
  (* insns *)
  sched insns []

let insn_typ insn =
  match insn with
  | Nop | Li _ | Fli _ | La _ | Add _ | Sub _ | Addi _ | Sll _ | Srl _ | Slli _
  | Srli _ ->
      (* ALU *) 1
  | Fadd _ | Fsub _ | Fmul _ | Fdiv _ | Fmv _ | Fmvxw _ | Fmvwx _ | Fneg _
  | Feq _ | Fle _ | Flt _ | IntOfFloat _ | FloatOfInt _ | Fabs _ | Floor _
  | Sqrt _ ->
      (* FPU *) 2
  | Lw _ | Flw _ | Sw _ | Fsw _ -> (* memory *) 3
  | Beq _ | Bne _ | Blt _ -> (* conditional branch *) 4
  | Jump _ | Jalr _ | Jal _ -> (* unconditional branch *) 5
  | Label _ -> (* label *) 6
(* [1/2/4/5, 1/2, 3, 3] *)

let print_unit oc insn =
  match insn with
  | Nop -> Printf.fprintf oc "nop; "
  | Li (r1, i) -> Printf.fprintf oc "li %s, %d; " r1 i
  | Fli (r1, f) -> Printf.fprintf oc "fli %s, %f; " r1 f
  | La (r1, l) -> Printf.fprintf oc "la %s, %s; " r1 l
  | Add (r1, r2, r3) -> Printf.fprintf oc "add %s, %s, %s; " r1 r2 r3
  | Sub (r1, r2, r3) -> Printf.fprintf oc "sub %s, %s, %s; " r1 r2 r3
  | Addi (r1, r2, i) -> Printf.fprintf oc "addi %s, %s, %d; " r1 r2 i
  | Sll (r1, r2, r3) -> Printf.fprintf oc "sll %s, %s, %s; " r1 r2 r3
  | Srl (r1, r2, r3) -> Printf.fprintf oc "srl %s, %s, %s; " r1 r2 r3
  | Slli (r1, r2, i) -> Printf.fprintf oc "slli %s, %s, %d; " r1 r2 i
  | Srli (r1, r2, i) -> Printf.fprintf oc "srli %s, %s, %d; " r1 r2 i
  | Fadd (r1, r2, r3) -> Printf.fprintf oc "fadd %s, %s, %s; " r1 r2 r3
  | Fsub (r1, r2, r3) -> Printf.fprintf oc "fsub %s, %s, %s; " r1 r2 r3
  | Fmul (r1, r2, r3) -> Printf.fprintf oc "fmul %s, %s, %s; " r1 r2 r3
  | Fdiv (r1, r2, r3) -> Printf.fprintf oc "fdiv %s, %s, %s; " r1 r2 r3
  | Fmv (r1, r2) -> Printf.fprintf oc "fmv %s, %s; " r1 r2
  | Fmvxw (r1, r2) -> Printf.fprintf oc "fmv.x.w %s, %s; " r1 r2
  | Fmvwx (r1, r2) -> Printf.fprintf oc "fmv.w.x %s, %s; " r1 r2
  | Fneg (r1, r2) -> Printf.fprintf oc "fneg %s, %s; " r1 r2
  | Fabs (r1, r2) -> Printf.fprintf oc "fabs %s, %s; " r1 r2
  | Floor (r1, r2) -> Printf.fprintf oc "floor %s, %s; " r1 r2
  | Lw (r1, i, r2) -> Printf.fprintf oc "lw %s, %d(%s); " r1 i r2
  | Flw (r1, i, r2) -> Printf.fprintf oc "flw %s, %d(%s); " r1 i r2
  | Sw (r1, i, r2) -> Printf.fprintf oc "sw %s, %d(%s); " r1 i r2
  | Fsw (r1, i, r2) -> Printf.fprintf oc "fsw %s, %d(%s); " r1 i r2
  | Beq (r1, r2, l) -> Printf.fprintf oc "beq %s, %s, %s; " r1 r2 l
  | Bne (r1, r2, l) -> Printf.fprintf oc "bne %s, %s, %s; " r1 r2 l
  | Blt (r1, r2, l) -> Printf.fprintf oc "blt %s, %s, %s; " r1 r2 l
  | Feq (r1, r2, r3) -> Printf.fprintf oc "feq %s, %s, %s; " r1 r2 r3
  | Fle (r1, r2, r3) -> Printf.fprintf oc "fle %s, %s, %s; " r1 r2 r3
  | Flt (r1, r2, r3) -> Printf.fprintf oc "flt %s, %s, %s; " r1 r2 r3
  | IntOfFloat (r1, r2) -> Printf.fprintf oc "ftoi %s, %s; " r1 r2
  | FloatOfInt (r1, r2) -> Printf.fprintf oc "itof %s, %s; " r1 r2
  | Sqrt (r1, r2) -> Printf.fprintf oc "fsqrt %s, %s; " r1 r2
  | Jump l -> Printf.fprintf oc "jump %s; " l
  | Jalr (r1, r2, i) -> Printf.fprintf oc "jalr %s, %s, %d; " r1 r2 i
  | Jal (r1, r2) -> Printf.fprintf oc "jal %s, %s; " r1 r2
  | Label l -> Printf.fprintf oc "%s:\n" l

let rec print_line oc tmp =
  Printf.fprintf oc "\t";
  print_unit oc tmp.(0);
  print_unit oc tmp.(1);
  print_unit oc tmp.(2);
  print_unit oc tmp.(3);
  Printf.fprintf oc "\n"

let rec print oc insns tmp =
  match insns with
  | Label l :: rest ->
      print_line oc tmp;
      Printf.fprintf oc "%s:\n" l;
      print oc rest [| Nop; Nop; Nop; Nop |]
  | cur :: rest ->
      let ty = insn_typ cur in
      if ty = 4 then (
        (* if tmp.(0) is _conditional_ branch,
           others are _after_ that *)
        print_line oc tmp;
        let tmp = [| Nop; Nop; Nop; Nop |] in
        tmp.(0) <- cur;
        print oc rest tmp)
      else if ty = 5 then
        (* if tmp.(0) is _unconditional_ branch,
             others are _before_ that *)
        if tmp.(0) = Nop then (
          tmp.(0) <- cur;
          print_line oc tmp;
          let tmp = [| Nop; Nop; Nop; Nop |] in
          print oc rest tmp)
        else (
          print_line oc tmp;
          let tmp = [| Nop; Nop; Nop; Nop |] in
          tmp.(0) <- cur;
          print_line oc tmp;
          let tmp = [| Nop; Nop; Nop; Nop |] in
          print oc rest tmp)
      else
        let depend =
          is_depend_raw tmp.(0) cur
          || is_depend_raw tmp.(1) cur
          || is_depend_raw tmp.(2) cur
          || is_depend_raw tmp.(3) cur
          || (ty = 1 && tmp.(0) != Nop && tmp.(1) != Nop)
          || (ty = 2 && tmp.(0) != Nop && tmp.(1) != Nop)
          || (ty = 3 && tmp.(2) != Nop && tmp.(3) != Nop)
        in
        if depend then (
          print_line oc tmp;
          let tmp = [| Nop; Nop; Nop; Nop |] in
          if ty = 1 then tmp.(1) <- cur
          else if ty = 2 then tmp.(1) <- cur
          else if ty = 3 then tmp.(2) <- cur
          else assert false;
          print oc rest tmp)
        else (
          if ty = 1 then
            if tmp.(1) = Nop then tmp.(1) <- cur else tmp.(0) <- cur
          else if ty = 2 then
            if tmp.(1) = Nop then tmp.(1) <- cur else tmp.(0) <- cur
          else if ty = 3 then
            if tmp.(2) = Nop then tmp.(2) <- cur else tmp.(3) <- cur
          else assert false;
          print oc rest tmp)
  | [] -> print_line oc tmp

let print_all oc insns =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc "\tli hp, %d\n" !Normalize.hp_init;
  Printf.fprintf oc "\tjump min_caml_start\n";
  let print_file filename =
    let chan = open_in filename in
    try
      while true do
        Printf.fprintf oc "%s\n" (input_line chan)
      done
    with End_of_file -> close_in chan
  in
  print_file "lib/print.s";
  print_file "lib/read.s";
  print_file "lib/float.s";
  print_file "lib/array.s";
  print_file "lib/tri.s";
  print oc insns [| Nop; Nop; Nop; Nop |]
