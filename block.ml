open Asm
open Ds

type t =
  | Nop of (Id.t * Type.t)
  | Set of (Id.t * Type.t) * int
  | SetF of (Id.t * Type.t) * float
  | SetL of (Id.t * Type.t) * Id.l
  | Mov of (Id.t * Type.t) * Id.t
  | Neg of (Id.t * Type.t) * Id.t
  | Add of (Id.t * Type.t) * Id.t * Asm.id_or_imm
  | Sub of (Id.t * Type.t) * Id.t * Asm.id_or_imm
  | SLL of (Id.t * Type.t) * Id.t * Asm.id_or_imm
  | SRL of (Id.t * Type.t) * Id.t * Asm.id_or_imm
  | Ld of (Id.t * Type.t) * Id.t * Asm.id_or_imm
  | St of (Id.t * Type.t) * Id.t * Id.t * Asm.id_or_imm
  | FMovD of (Id.t * Type.t) * Id.t
  | FNegD of (Id.t * Type.t) * Id.t
  | FAddD of (Id.t * Type.t) * Id.t * Id.t
  | FSubD of (Id.t * Type.t) * Id.t * Id.t
  | FMulD of (Id.t * Type.t) * Id.t * Id.t
  | FDivD of (Id.t * Type.t) * Id.t * Id.t
  | LdDF of (Id.t * Type.t) * Id.t * Asm.id_or_imm
  | StDF of (Id.t * Type.t) * Id.t * Id.t * Asm.id_or_imm
  | Comment of string
  | IfEq of (Id.t * Type.t) * Id.t * Asm.id_or_imm * Id.t * Id.t
  | IfLE of (Id.t * Type.t) * Id.t * Asm.id_or_imm * Id.t * Id.t
  | IfGE of (Id.t * Type.t) * Id.t * Asm.id_or_imm * Id.t * Id.t
  | IfFEq of (Id.t * Type.t) * Id.t * Id.t * Id.t * Id.t
  | IfFLE of (Id.t * Type.t) * Id.t * Id.t * Id.t * Id.t
  | CallCls of (Id.t * Type.t) * Id.t * Id.t list * Id.t list
  | CallDir of (Id.t * Type.t) * Id.l * Id.t list * Id.t list
  | Save of (Id.t * Type.t) * Id.t * Id.t
  | Restore of (Id.t * Type.t) * Id.t
  | Fiszero of (Id.t * Type.t) * Id.t
  | Fispos of (Id.t * Type.t) * Id.t
  | Fisneg of (Id.t * Type.t) * Id.t
  | Fneg of (Id.t * Type.t) * Id.t
  | Fless of (Id.t * Type.t) * Id.t * Id.t
  | IntOfFloat of (Id.t * Type.t) * Id.t
  | FloatOfInt of (Id.t * Type.t) * Id.t
  | Sqrt of (Id.t * Type.t) * Id.t
  | Fsqr of (Id.t * Type.t) * Id.t

type instr = {
  mutable id : Id.t;
  mutable block : Id.t;
  mutable instr : t;
  mutable liveIn : Id.t list;
  mutable liveOut : Id.t list;
  mutable nextId : Id.t;
  mutable predId : Id.t;
}

type block = {
  mutable id : Id.t;
  mutable kansu : Id.l;
  mutable instrs : (Id.t * instr) list;
  mutable head : Id.t;
  mutable tail : Id.t;
  mutable predIds : Id.t list;
  mutable nextIds : Id.t list;
  mutable liveIn : Id.t list;
  mutable liveOut : Id.t list;
  mutable useInside : Id.t list;
  mutable defInside : Id.t list;
}

type fundef = {
  mutable name : Id.l;
  mutable args : Id.t list;
  mutable fargs : Id.t list;
  mutable ret : Type.t;
  mutable blocks : (Id.t * block) list;
  mutable head : Id.t;
}

type prog = Prog of (Id.l * float) list * fundef list * fundef

let rec erase_zero li =
  match li with
  | [] -> []
  | l :: ls ->
      if l = "zero" || l = "fzero" then erase_zero ls else l :: erase_zero ls

let def_use instr =
  match instr.instr with
  | Nop dest
  | Set (dest, _)
  | SetF (dest, _)
  | SetL (dest, _)
  | Restore (dest, _) ->
      (erase_zero [ fst dest ], [])
  | Mov (dest, x)
  | Neg (dest, x)
  | Add (dest, x, C _)
  | Sub (dest, x, C _)
  | SLL (dest, x, C _)
  | SRL (dest, x, C _)
  | Ld (dest, x, C _)
  | FMovD (dest, x)
  | FNegD (dest, x)
  | LdDF (dest, x, C _)
  | IfEq (dest, x, C _, _, _)
  | IfLE (dest, x, C _, _, _)
  | IfGE (dest, x, C _, _, _)
  | Save (dest, x, _)
  | Fiszero (dest, x)
  | Fispos (dest, x)
  | Fisneg (dest, x)
  | Fneg (dest, x)
  | IntOfFloat (dest, x)
  | FloatOfInt (dest, x)
  | Sqrt (dest, x)
  | Fsqr (dest, x) ->
      (erase_zero [ fst dest ], erase_zero [ x ])
  | Add (dest, x, V y)
  | Sub (dest, x, V y)
  | SLL (dest, x, V y)
  | SRL (dest, x, V y)
  | Ld (dest, x, V y)
  | St (dest, x, y, C _)
  | FAddD (dest, x, y)
  | FSubD (dest, x, y)
  | FMulD (dest, x, y)
  | FDivD (dest, x, y)
  | LdDF (dest, x, V y)
  | StDF (dest, x, y, C _)
  | IfEq (dest, x, V y, _, _)
  | IfLE (dest, x, V y, _, _)
  | IfGE (dest, x, V y, _, _)
  | IfFEq (dest, x, y, _, _)
  | IfFLE (dest, x, y, _, _)
  | Fless (dest, x, y) ->
      (erase_zero [ fst dest ], erase_zero [ x; y ])
  | St (dest, x, y, V z) | StDF (dest, x, y, V z) ->
      (erase_zero [ fst dest ], erase_zero [ x; y; z ])
  | CallCls (dest, name, args, fargs) ->
      (erase_zero [ fst dest ], erase_zero (args @ fargs))
  | CallDir (dest, Id.L name, args, fargs) ->
      (erase_zero [ fst dest ], erase_zero (args @ fargs))

let new_block id pred next =
  {
    id;
    kansu = Id.L "";
    instrs = [];
    predIds = pred;
    nextIds = next;
    head = "";
    tail = "";
    liveIn = [];
    liveOut = [];
    useInside = [];
    defInside = [];
  }

let pred_instr_id = ref None

let block_count = ref 0

let instr_count = ref 0

let gen_block_id () =
  block_count := !block_count + 1;
  Printf.sprintf "block.%d" !block_count

let gen_instr_id () =
  instr_count := !instr_count + 1;
  Printf.sprintf "instr.%d" !instr_count

let def_place = ref []

let use_place = ref []

(*fundにブロックを追加し、predのブロックのnextにこのブロックを追加*)
let remember_place_of_us_df_init () =
  def_place := [];
  use_place := []

let remember_place_of_us_df block =
  let rec sub instr_id =
    let instr = env_find instr_id block.instrs in
    let df, us = def_use instr in
    List.iter
      (fun d ->
        let places = try env_find d !def_place with Not_found -> [] in
        def_place := env_add d ((block.id, instr.id) :: places) !def_place)
      df;
    List.iter
      (fun d ->
        let places = try env_find d !use_place with Not_found -> [] in
        use_place := env_add d ((block.id, instr.id) :: places) !use_place)
      us;
    if instr.predId <> "" then sub instr.predId else ()
  in
  if not (block.instrs = []) then sub block.tail

let get_def_place i = try env_find i !def_place with Not_found -> []

let get_use_place i = try env_find i !use_place with Not_found -> []

let rec add_block n_block fund =
  List.iter
    (fun id ->
      let pred = env_find id fund.blocks in
      pred.nextIds <- n_block.id :: pred.nextIds)
    n_block.predIds;
  n_block.kansu <- fund.name;
  fund.blocks <- env_add n_block.id n_block fund.blocks

let rec add_instr t block =
  let id = gen_instr_id () in
  let instr =
    {
      id;
      block = block.id;
      instr = t;
      liveIn = [];
      liveOut = [];
      predId = "";
      nextId = "";
    }
  in
  (if block.instrs = [] then block.head <- id
  else
    match !pred_instr_id with
    | None -> assert false
    | Some pred ->
        pred.nextId <- instr.id;
        instr.predId <- pred.id);
  pred_instr_id := Some instr;
  block.instrs <- env_add id instr block.instrs;
  block.tail <- id

let rec g fundef block dest = function
  | Asm.Ans exp ->
      let n_block = g' fundef block dest exp in
      add_block n_block fundef;
      n_block
  | Asm.Let ((x, t), exp, e) ->
      let n_block = g' fundef block (x, t) exp in
      g fundef n_block dest e

and g' fund block dest = function
  | Asm.Nop ->
      add_instr (Nop dest) block;
      block
  | Asm.Set x ->
      add_instr (Set (dest, x)) block;
      block
  | Asm.SetF x ->
      add_instr (SetF (dest, x)) block;
      block
  | Asm.SetL x ->
      add_instr (SetL (dest, x)) block;
      block
  | Asm.Mov x ->
      add_instr (Mov (dest, x)) block;
      block
  | Asm.Neg x ->
      add_instr (Neg (dest, x)) block;
      block
  | Asm.Add (x, y') ->
      add_instr (Add (dest, x, y')) block;
      block
  | Asm.Sub (x, y') ->
      add_instr (Sub (dest, x, y')) block;
      block
  | Asm.SLL (x, y') ->
      add_instr (SLL (dest, x, y')) block;
      block
  | Asm.SRL (x, y') ->
      add_instr (SRL (dest, x, y')) block;
      block
  | Asm.Ld (x, y') ->
      add_instr (Ld (dest, x, y')) block;
      block
  | Asm.St (x, y, z') ->
      add_instr (St (dest, x, y, z')) block;
      block
  | Asm.FMovD x ->
      add_instr (FMovD (dest, x)) block;
      block
  | Asm.FNegD x ->
      add_instr (FNegD (dest, x)) block;
      block
  | Asm.FAddD (x, y) ->
      add_instr (FAddD (dest, x, y)) block;
      block
  | Asm.FSubD (x, y) ->
      add_instr (FSubD (dest, x, y)) block;
      block
  | Asm.FMulD (x, y) ->
      add_instr (FMulD (dest, x, y)) block;
      block
  | Asm.FDivD (x, y) ->
      add_instr (FDivD (dest, x, y)) block;
      block
  | Asm.LdDF (x, y') ->
      add_instr (LdDF (dest, x, y')) block;
      block
  | Asm.StDF (x, y, z') ->
      add_instr (StDF (dest, x, y, z')) block;
      block
  | Asm.IfEq (x, y', e1, e2) ->
      let b1_id = gen_block_id () in
      let b2_id = gen_block_id () in
      let m_id = gen_block_id () in
      add_instr (IfEq (dest, x, y', b1_id, b2_id)) block;
      add_block block fund;
      let b1 = new_block b1_id [ block.id ] [] in
      let final_b1 = g fund b1 dest e1 in
      let b2 = new_block b2_id [ block.id ] [] in
      let final_b2 = g fund b2 dest e2 in
      let m = new_block m_id [ final_b1.id; final_b2.id ] [] in
      m
  | Asm.IfLE (x, y', e1, e2) ->
      let b1_id = gen_block_id () in
      let b2_id = gen_block_id () in
      let m_id = gen_block_id () in
      add_instr (IfLE (dest, x, y', b1_id, b2_id)) block;
      add_block block fund;
      let b1 = new_block b1_id [ block.id ] [] in
      let final_b1 = g fund b1 dest e1 in
      let b2 = new_block b2_id [ block.id ] [] in
      let final_b2 = g fund b2 dest e2 in
      let m = new_block m_id [ final_b1.id; final_b2.id ] [] in
      m
  | Asm.IfGE (x, y', e1, e2) ->
      let b1_id = gen_block_id () in
      let b2_id = gen_block_id () in
      let m_id = gen_block_id () in
      add_instr (IfGE (dest, x, y', b1_id, b2_id)) block;
      add_block block fund;
      let b1 = new_block b1_id [ block.id ] [] in
      let final_b1 = g fund b1 dest e1 in
      let b2 = new_block b2_id [ block.id ] [] in
      let final_b2 = g fund b2 dest e2 in
      let m = new_block m_id [ final_b1.id; final_b2.id ] [] in
      m
  | Asm.IfFEq (x, y, e1, e2) ->
      let b1_id = gen_block_id () in
      let b2_id = gen_block_id () in
      let m_id = gen_block_id () in
      add_instr (IfFEq (dest, x, y, b1_id, b2_id)) block;
      add_block block fund;
      let b1 = new_block b1_id [ block.id ] [] in
      let final_b1 = g fund b1 dest e1 in
      let b2 = new_block b2_id [ block.id ] [] in
      let final_b2 = g fund b2 dest e2 in
      let m = new_block m_id [ final_b1.id; final_b2.id ] [] in
      m
  | Asm.IfFLE (x, y, e1, e2) ->
      let b1_id = gen_block_id () in
      let b2_id = gen_block_id () in
      let m_id = gen_block_id () in
      add_instr (IfFLE (dest, x, y, b1_id, b2_id)) block;
      add_block block fund;
      let b1 = new_block b1_id [ block.id ] [] in
      let final_b1 = g fund b1 dest e1 in
      let b2 = new_block b2_id [ block.id ] [] in
      let final_b2 = g fund b2 dest e2 in
      let m = new_block m_id [ final_b1.id; final_b2.id ] [] in
      m
  | Asm.CallCls (x, ys, zs) ->
      add_instr (CallCls (dest, x, ys, zs)) block;
      block
  | Asm.CallDir (Id.L x, ys, zs) ->
      add_instr (CallDir (dest, Id.L x, ys, zs)) block;
      block
  | Asm.Save (x, y) ->
      add_instr (Save (dest, x, y)) block;
      block
  | Asm.Restore x ->
      add_instr (Restore (dest, x)) block;
      block
  | Asm.Comment _ -> block
  | Asm.Fiszero x ->
      add_instr (Fiszero (dest, x)) block;
      block
  | Asm.Fispos x ->
      add_instr (Fispos (dest, x)) block;
      block
  | Asm.Fisneg x ->
      add_instr (Fisneg (dest, x)) block;
      block
  | Asm.Fneg x ->
      add_instr (Fneg (dest, x)) block;
      block
  | Asm.Fless (x, y) ->
      add_instr (Fless (dest, x, y)) block;
      block
  | Asm.IntOfFloat x ->
      add_instr (IntOfFloat (dest, x)) block;
      block
  | Asm.FloatOfInt x ->
      add_instr (FloatOfInt (dest, x)) block;
      block
  | Asm.Sqrt x ->
      add_instr (Sqrt (dest, x)) block;
      block
  | Asm.Fsqr x ->
      add_instr (Fsqr (dest, x)) block;
      block

let h
    {
      Asm.name = Id.L x;
      Asm.args = ys;
      Asm.fargs = zs;
      Asm.body = e;
      Asm.ret = t;
    } =
  let dummy_block = new_block (gen_block_id ()) [] [] in
  let fund =
    {
      name = Id.L x;
      args = ys;
      fargs = zs;
      ret = t;
      blocks = [];
      head = dummy_block.id;
    }
  in
  let ret_reg =
    match t with
    | Type.Unit -> Id.gentmp Type.Unit
    | Type.Float -> Asm.fregs.(0)
    | _ -> regs.(0)
  in
  g fund dummy_block (ret_reg, t) e;
  fund

let f (Asm.Prog (data, fundefs, e)) =
  Format.eprintf "create kihon block@.";
  Prog
    ( data,
      List.map h fundefs,
      h
        {
          name = Id.L "min_caml_start";
          args = [];
          fargs = [];
          body = e;
          ret = Type.Unit;
        } )

let replace instr x y =
  let rep a = if a = x then y else a in
  let rr (a, t) = (rep a, t) in
  match instr.instr with
  | Nop dest -> Nop (rr dest)
  | Set (dest, x) -> Set (rr dest, x)
  | SetF (dest, x) -> SetF (rr dest, x)
  | SetL (dest, Id.L x) -> SetL (rr dest, Id.L x)
  | Mov (dest, x) -> Mov (rr dest, rep x)
  | Neg (dest, x) -> Neg (rr dest, rep x)
  | Add (dest, x, V y) -> Add (rr dest, rep x, V (rep y))
  | Sub (dest, x, V y) -> Sub (rr dest, rep x, V (rep y))
  | SLL (dest, x, V y) -> SLL (rr dest, rep x, V (rep y))
  | SRL (dest, x, V y) -> SRL (rr dest, rep x, V (rep y))
  | Add (dest, x, C y) -> Add (rr dest, rep x, C y)
  | Sub (dest, x, C y) -> Sub (rr dest, rep x, C y)
  | SLL (dest, x, C y) -> SLL (rr dest, rep x, C y)
  | SRL (dest, x, C y) -> SRL (rr dest, rep x, C y)
  | Ld (dest, x, V y) -> Ld (rr dest, rep x, V (rep y))
  | St (dest, x, y, V z) -> St (rr dest, rep x, rep y, V (rep z))
  | Ld (dest, x, C y) -> Ld (rr dest, rep x, C y)
  | St (dest, x, y, C z) -> St (rr dest, rep x, rep y, C z)
  | FMovD (dest, x) -> FMovD (rr dest, rep x)
  | FNegD (dest, x) -> FNegD (rr dest, rep x)
  | FAddD (dest, x, y) -> FAddD (rr dest, rep x, rep y)
  | FSubD (dest, x, y) -> FSubD (rr dest, rep x, rep y)
  | FMulD (dest, x, y) -> FMulD (rr dest, rep x, rep y)
  | FDivD (dest, x, y) -> FDivD (rr dest, rep x, rep y)
  | LdDF (dest, x, V y) -> LdDF (rr dest, rep x, V (rep y))
  | StDF (dest, x, y, V z) -> StDF (rr dest, rep x, rep y, V (rep z))
  | LdDF (dest, x, C y) -> LdDF (rr dest, rep x, C y)
  | StDF (dest, x, y, C z) -> StDF (rr dest, rep x, rep y, C z)
  | IfEq (dest, x, V y, b1, b2) -> IfEq (rr dest, rep x, V (rep y), b1, b2)
  | IfLE (dest, x, V y, b1, b2) -> IfLE (rr dest, rep x, V (rep y), b1, b2)
  | IfGE (dest, x, V y, b1, b2) -> IfGE (rr dest, rep x, V (rep y), b1, b2)
  | IfEq (dest, x, C y, b1, b2) -> IfEq (rr dest, rep x, C y, b1, b2)
  | IfLE (dest, x, C y, b1, b2) -> IfLE (rr dest, rep x, C y, b1, b2)
  | IfGE (dest, x, C y, b1, b2) -> IfGE (rr dest, rep x, C y, b1, b2)
  | IfFEq (dest, x, y, b1, b2) -> IfFEq (rr dest, rep x, rep y, b1, b2)
  | IfFLE (dest, x, y, b1, b2) -> IfFLE (rr dest, rep x, rep y, b1, b2)
  | CallCls (dest, name, args, fargs) -> assert false
  | CallDir (dest, Id.L name, args, fargs) ->
      CallDir (rr dest, Id.L name, List.map rep args, List.map rep fargs)
  | Save (dest, x, y) -> Save (rr dest, rep x, y)
  | Restore (dest, x) -> Restore (rr dest, x)
  | Fiszero (dest, x) -> Fiszero (rr dest, x)
  | Fispos (dest, x) -> Fispos (rr dest, x)
  | Fisneg (dest, x) -> Fisneg (rr dest, x)
  | Fneg (dest, x) -> Fneg (rr dest, x)
  | Fless (dest, x, y) -> Fless (rr dest, x, y)
  | IntOfFloat (dest, x) -> IntOfFloat (rr dest, x)
  | FloatOfInt (dest, x) -> FloatOfInt (rr dest, x)
  | Sqrt (dest, x) -> Sqrt (rr dest, x)
  | Fsqr (dest, x) -> Fsqr (rr dest, x)

type fund_dep = {
  fname : Id.l;
  mutable calls : Id.l list;
  mutable calleds : Id.l list;
}

let rec find_calls fund =
  let ans = ref [] in
  env_map
    (fun block ->
      env_map
        (fun instr ->
          match instr.instr with
          | CallDir (_, name, _, _) -> ans := name :: !ans
          (* | CallCls (_, name, _, _) -> ans := name :: !ans *)
          | _ -> ())
        block.instrs)
    fund.blocks;
  !ans

let rec find_min_fund all_list =
  let min = ref 100000 in
  let f = ref { fname = Id.L ""; calls = []; calleds = [] } in
  List.map
    (fun fund_dep ->
      if List.length fund_dep.calls < !min then (
        min := List.length fund_dep.calls;
        f := fund_dep)
      else ())
    all_list;
  assert (!f.fname <> Id.L "");
  !f

let rec list_remove i li =
  match li with
  | [] -> []
  | l :: ls -> if l = i then list_remove i ls else l :: list_remove i ls

let rec removing a all_list =
  match all_list with
  | [] -> []
  | b :: bs ->
      let c = list_remove a.fname b.calls in
      b.calls <- c;
      b :: removing a bs

let rec analyze_dependency fundefs =
  let all_list =
    List.map
      (fun fund -> { fname = fund.name; calls = []; calleds = [] })
      fundefs
  in
  let all_m =
    List.fold_left
      (fun env fund -> env_add (Id.get_name fund.fname) fund env)
      [] all_list
  in
  List.map
    (fun fund ->
      let call_list = find_calls fund in
      let fund_m = env_find (Id.get_name fund.name) all_m in
      fund_m.calls <- call_list)
    fundefs;
  let all_list =
    ref (List.map (fun f -> env_find (Id.get_name f.fname) all_m) all_list)
  in
  let result = ref [] in
  while List.length !all_list > 0 do
    let a = find_min_fund !all_list in
    all_list := removing a !all_list;
    result := Id.get_name a.fname :: !result
  done;
  List.rev !result

(*なるべく呼び出しが少ないものから*)
let rec sort_fundefs fundefs =
  let m_fundefs =
    List.fold_left
      (fun env fund -> env_add (Id.get_name fund.name) fund env)
      [] fundefs
  in
  let fund_names = analyze_dependency fundefs in
  List.map (fun name -> env_find name m_fundefs) fund_names
