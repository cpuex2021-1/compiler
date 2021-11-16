open Ds
open Asm

external gethi : float -> int32 = "gethi"

external getlo : float -> int32 = "getlo"

let stackset = ref [] (* すでにSaveされた変数の集合 *)

let stackmap = ref []
(* Saveされた変数の、スタックにおける位置 *)

let save x =
  stackset := set_add x !stackset;
  if not (List.mem x !stackmap) then stackmap := !stackmap @ [ x ]

let savef x =
  stackset := set_add x !stackset;
  if not (List.mem x !stackmap) then
    let pad =
      if List.length !stackmap mod 2 = 0 then [] else [ Id.gentmp Type.Int ]
    in
    stackmap := !stackmap @ pad @ [ x; x ]

let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs)
  in
  loc !stackmap

let offset x = 1 * List.hd (locate x)

let stacksize () = align ((List.length !stackmap + 1) * 1)

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
      (y, sw)
      ::
      (x, y)
      ::
      shuffle sw
        (List.map (function y', z when y = y' -> (sw, z) | yz -> yz) xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t
(* 末尾かどうかを表すデータ型 *)

let rec g oc = function
  (* 命令列のアセンブリ生成 *)
  | dest, Ans exp -> g' oc (dest, exp)
  | dest, Let ((x, t), exp, e) ->
      g' oc (NonTail x, exp);
      g oc (dest, e)

and g' oc = function
  (* 各命令のアセンブリ生成 *)
  (* 末尾でなかったら計算結果をdestにセット *)
  | NonTail _, Nop -> ()
  | NonTail x, Set i -> Printf.fprintf oc "\taddi %s, zero, %d\n" x i
  | NonTail x, SetL (Id.L y) -> Printf.fprintf oc "\taddi %s, zero, %s\n" x y
  | NonTail x, Mov y when x = y -> ()
  | NonTail x, Mov y -> Printf.fprintf oc "\tadd %s, %s, zero\n" x y
  | NonTail x, Neg y -> Printf.fprintf oc "\tsub %s, zero, %s\n" x y
  | NonTail x, Add (y, z') -> (
      match z' with
      | V id -> Printf.fprintf oc "\tadd %s, %s, %s\n" x y id
      | C i -> Printf.fprintf oc "\taddi %s, %s, %d\n" x y i)
  | NonTail x, Sub (y, z') -> (
      match z' with
      | V id -> Printf.fprintf oc "\tsub %s, %s, %s\n" x y id
      | C i -> Printf.fprintf oc "\taddi %s, %s, %d\n" x y (-1 * i))
  | NonTail x, SLL (y, z') -> (
      match z' with
      | V id -> Printf.fprintf oc "\tsll %s, %s, %s\n" x y id
      | C i -> Printf.fprintf oc "\tslli %s, %s, %d\n" x y i)
  | NonTail x, Ld (y, z') -> (
      match z' with
      | V id -> Printf.fprintf oc "load offset must be immediate\n"
      | C i -> Printf.fprintf oc "\tlw %s, %d(%s)\n" x i y)
  | NonTail _, St (x, y, z') -> (
      match z' with
      | V id -> Printf.fprintf oc "store offset must be immediate\n"
      | C i -> Printf.fprintf oc "\tsw %s, %d(%s)\n" x i y)
  | NonTail x, FMovD y when x = y -> ()
  | NonTail x, FMovD y -> Printf.fprintf oc "\tfadd %s, %s, fzero\n" x y
  | NonTail x, FNegD y -> Printf.fprintf oc "\tfneg %s, %s\n" x y
  | NonTail x, FAddD (y, z) -> Printf.fprintf oc "\tfadd %s, %s, %s\n" x y z
  | NonTail x, FSubD (y, z) -> Printf.fprintf oc "\tfsub %s, %s, %s\n" x y z
  | NonTail x, FMulD (y, z) -> Printf.fprintf oc "\tfmul %s, %s, %s\n" x y z
  | NonTail x, FDivD (y, z) -> Printf.fprintf oc "\tfdiv %s, %s, %s\n" x y z
  | NonTail x, LdDF (y, z') -> (
      match z' with
      | V id -> Printf.fprintf oc "load offset must be immediate\n"
      | C i -> Printf.fprintf oc "\tflw %s, %d(%s)\n" x i y)
  | NonTail _, StDF (x, y, z') -> (
      match z' with
      | V id -> Printf.fprintf oc "store offset must be immediate\n"
      | C i -> Printf.fprintf oc "\tfsw %s, %d(%s)\n" x i y)
  | NonTail _, Comment s -> Printf.fprintf oc "\t! %s\n" s
  (* 退避の仮想命令の実装 *)
  | NonTail _, Save (x, y)
    when List.mem x allregs && not (set_exist y !stackset) ->
      save y;
      Printf.fprintf oc "\tsw %s, %d(%s)\n" x (offset y) reg_sp
  | NonTail _, Save (x, y)
    when List.mem x allfregs && not (set_exist y !stackset) ->
      savef y;
      Printf.fprintf oc "\tsw %s, %d(%s)\n" x (offset y) reg_sp
  | NonTail _, Save (x, y) ->
      assert (set_exist y !stackset);
      ()
  (* 復帰の仮想命令の実装 *)
  | NonTail x, Restore y when List.mem x allregs ->
      Printf.fprintf oc "\tlw %s, %d(%s)\n" x (offset y) reg_sp
  | NonTail x, Restore y ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tlw %s, %d(%s)\n" x (offset y) reg_sp
  (* 末尾だったら計算結果を第一レジスタにセットしてret *)
  | Tail, ((Nop | St _ | StDF _ | Comment _ | Save _) as exp) ->
      g' oc (NonTail (Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tjalr zero, ra, 0 ! ret\n"
  | ( Tail,
      ((Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | SLL _ | Ld _) as exp) )
    ->
      g' oc (NonTail regs.(0), exp);
      Printf.fprintf oc "\tjalr zero, ra, 0 ! ret\n"
  | ( Tail,
      ((FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _) as
      exp) ) ->
      g' oc (NonTail fregs.(0), exp);
      Printf.fprintf oc "\tjalr zero, ra, 0 ! ret\n"
  | Tail, (Restore x as exp) ->
      (match locate x with
      | [ i ] -> g' oc (NonTail regs.(0), exp)
      | [ i; j ] when i + 1 = j -> g' oc (NonTail fregs.(0), exp)
      | _ -> assert false);
      Printf.fprintf oc "\tjalr zero, ra, 0 ! ret\n"
  | Tail, IfEq (x, y', e1, e2) ->
      let b_else = Id.genid "be_else" in
      Printf.fprintf oc "\tbne %s, %s, %s\n" x (pp_id_or_imm y') b_else;
      let stackset_back = !stackset in
      g oc (Tail, e1);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e2)
  | Tail, IfLE (x, y', e1, e2) ->
      let b_else = Id.genid "ble_else" in
      Printf.fprintf oc "\tblt %s, %s, %s\n" (pp_id_or_imm y') x b_else;
      let stackset_back = !stackset in
      g oc (Tail, e1);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e2)
  | Tail, IfGE (x, y', e1, e2) ->
      let b_else = Id.genid "bge_else" in
      Printf.fprintf oc "\tblt %s, %s, %s\n" x (pp_id_or_imm y') b_else;
      let stackset_back = !stackset in
      g oc (Tail, e1);
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (Tail, e2)
  | Tail, IfFEq (x, y, e1, e2) -> Printf.fprintf oc "IfFEq not supported\n"
  | Tail, IfFLE (x, y, e1, e2) -> Printf.fprintf oc "IfFLE not supported\n"
  | NonTail z, IfEq (x, y', e1, e2) ->
      let b_else = Id.genid "be_else" in
      let b_cont = Id.genid "be_cont" in
      Printf.fprintf oc "\tbne %s, %s, %s\n" x (pp_id_or_imm y') b_else;
      let stackset_back = !stackset in
      g oc (NonTail z, e1);
      let stackset1 = !stackset in
      Printf.fprintf oc "\t jump %s\n" b_cont;
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (NonTail z, e2);
      Printf.fprintf oc "%s:\n" b_cont;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  | NonTail z, IfLE (x, y', e1, e2) ->
      let b_else = Id.genid "ble_else" in
      let b_cont = Id.genid "ble_cont" in
      Printf.fprintf oc "\tblt %s, %s, %s\n" (pp_id_or_imm y') x b_else;
      let stackset_back = !stackset in
      g oc (NonTail z, e1);
      let stackset1 = !stackset in
      Printf.fprintf oc "\t jump %s\n" b_cont;
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (NonTail z, e2);
      Printf.fprintf oc "%s:\n" b_cont;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  | NonTail z, IfGE (x, y', e1, e2) ->
      let b_else = Id.genid "bge_else" in
      let b_cont = Id.genid "bge_cont" in
      Printf.fprintf oc "\tblt %s, %s, %s\n" x (pp_id_or_imm y') b_else;
      let stackset_back = !stackset in
      g oc (NonTail z, e1);
      let stackset1 = !stackset in
      Printf.fprintf oc "\t jump %s\n" b_cont;
      Printf.fprintf oc "%s:\n" b_else;
      stackset := stackset_back;
      g oc (NonTail z, e2);
      Printf.fprintf oc "%s:\n" b_cont;
      let stackset2 = !stackset in
      stackset := set_inter stackset1 stackset2
  | NonTail z, IfFEq (x, y, e1, e2) -> Printf.fprintf oc "IfFEq not supported\n"
  | NonTail z, IfFLE (x, y, e1, e2) -> Printf.fprintf oc "IfFLE not supported\n"
  (* 関数呼び出しの仮想命令の実装 *)
  | Tail, CallCls (x, ys, zs) ->
      (* 末尾呼び出し *)
      g'_args oc [ (x, reg_cl) ] ys zs;
      Printf.fprintf oc "\tlw %s, 0(%s)\n" reg_sw reg_cl;
      Printf.fprintf oc "\tjalr zero, ra, 0\n"
  | Tail, CallDir (Id.L x, ys, zs) ->
      (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tjump %s\n" x
  | NonTail a, CallCls (x, ys, zs) ->
      g'_args oc [ (x, reg_cl) ] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tsw %s, %d(%s)\n" reg_ra (ss - 1) reg_sp;
      Printf.fprintf oc "\tlw %s, 0(%s)\n" reg_sw reg_cl;
      Printf.fprintf oc "\tjal ra, %s ! call\n" reg_sw;
      (* Printf.fprintf oc "\taddi %s, %s, %d\n" reg_sp reg_sp (-1 * ss); *)
      Printf.fprintf oc "\tlw %s, %d(%s)\n" reg_ra (ss - 1) reg_sp;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tadd %s, %s, zero\n" a regs.(0)
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tfadd %s, %s, fzero\n" a regs.(0)
  | NonTail a, CallDir (Id.L x, ys, zs) ->
      g'_args oc [] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tsw %s, %d(%s)\n" reg_ra (ss - 1) reg_sp;
      Printf.fprintf oc "\tjal ra, %s ! call\n" x;
      (* Printf.fprintf oc "\taddi %s, %s, %d\n" reg_sp reg_sp (-1 * ss); *)
      Printf.fprintf oc "\tlw %s, %d(%s)\n" reg_ra (ss - 1) reg_sp;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tadd %s, %s, zero\n" a regs.(0)
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tfadd %s, %s, fzero\n" a regs.(0)

and g'_tail_if oc e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)

and g'_non_tail_if oc dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tb\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := set_inter stackset1 stackset2

and g'_args oc x_reg_cl ys zs =
  let i, yrs =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl) ys
  in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\tadd %s, %s, zero\n" r y)
    (shuffle reg_sw yrs);
  let d, zfrs =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, []) zs
  in
  List.iter
    (fun (z, fr) ->
      Printf.fprintf oc "\tfmv %s, %s\n" z fr;
      Printf.fprintf oc "\tfmv %s, %s\n" (co_freg z) (co_freg fr))
    (shuffle reg_fsw zfrs)

let h oc { name = Id.L x; args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := [];
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog (data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc "\tjump min_caml_start\n";
  Printf.fprintf oc "print_int:\n";
  Printf.fprintf oc "\tlw s0, 0(zero)\n";
  Printf.fprintf oc "\tjalr zero, ra, 0 ! ret\n";
  List.iter
    (fun (Id.L x, d) ->
      Printf.fprintf oc "%s:\t! %f\n" x d
      (*
      Printf.fprintf oc "\t.long\t0x%lx\n" (gethi d);
      Printf.fprintf oc "\t.long\t0x%lx\n" (getlo d)
      *))
    data;
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "min_caml_start:\n";
  Printf.fprintf oc "\taddi sp, sp, -28\n";
  (* from gcc; why 112? -> changed to 28 (for no reason)*)
  stackset := [];
  stackmap := [];
  g oc (NonTail "%g0", e);
  Printf.fprintf oc "\tjalr zero, ra, 0 ! ret\n"
