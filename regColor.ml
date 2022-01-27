open Block
open Ds

let cc = ref 0

let varenv = ref []

let allregs = Asm.allregs

let allfregs = Asm.allfregs

let precolored = ref []

let initial = ref []

let simplify_worklist = ref []

let freeze_worklist = ref []

let spill_worklist = ref []

let spilled_nodes = ref []

let coalesced_nodes = ref []

let colored_nodes = ref []

let select_stack = ref []

let coalesced_moves = ref []

let constrained_moves = ref []

let frozen_moves = ref []

let worklist_moves = ref []

let active_moves = ref []

let move_list = ref []

let all_moves = ref []

let alias = ref []

let adj_set = ref []

let adj_list = ref []

let degree = ref []

let color = ref []

let colorenv = ref []

let new_temps = ref []

let spill_cnt = ref 0

let ret_nodes = ref []

let arg_nodes = ref []

let striding_nodes = ref []

let is_loop = ref false

type fundata = { arg_regs : Id.t list; ret_reg : Id.t; use_regs : Id.t list }

let fundata = ref []

let rec erase_zero ls =
  match ls with
  | [] -> []
  | (a, b) :: li ->
      if a = "zero" || a = "fzero" then erase_zero li
      else (a, b) :: erase_zero li

let get_tp instr =
  match instr.instr with
  | Nop xt | Set (xt, _) | SetF (xt, _) | SetL (xt, _) | Restore (xt, _) ->
      erase_zero [ xt ]
  | Mov (xt, x)
  | Neg (xt, x)
  | Add (xt, x, Asm.C _)
  | Sub (xt, x, Asm.C _)
  | SLL (xt, x, Asm.C _)
  | SRL (xt, x, Asm.C _)
  | Ld (xt, x, Asm.C _)
  | LdDF (xt, x, Asm.C _)
  | IfEq (xt, x, Asm.C _, _, _)
  | IfLE (xt, x, Asm.C _, _, _)
  | IfGE (xt, x, Asm.C _, _, _) ->
      erase_zero [ xt; (x, Type.Int) ]
  | Sqrt (xt, x)
  | Fsqr (xt, x)
  | FMovD (xt, x)
  | FNegD (xt, x)
  | Fispos (xt, x)
  | Fispos (xt, x)
  | Fisneg (xt, x)
  | Fneg (xt, x)
  | IntOfFloat (xt, x) ->
      erase_zero [ xt; (x, Type.Float) ]
  | Save (xt, x, _) -> []
  | Add (xt, x, Asm.V y)
  | Sub (xt, x, Asm.V y)
  | SLL (xt, x, Asm.V y)
  | SRL (xt, x, Asm.V y)
  | Ld (xt, x, Asm.V y)
  | LdDF (xt, x, Asm.V y)
  | St (xt, x, y, Asm.C _)
  | IfEq (xt, x, Asm.V y, _, _)
  | IfLE (xt, x, Asm.V y, _, _)
  | IfGE (xt, x, Asm.V y, _, _)
  | IfFEq (xt, x, y, _, _)
  | IfFLE (xt, x, y, _, _) ->
      erase_zero [ xt; (x, Type.Int); (y, Type.Int) ]
  | Fless (xt, x, y)
  | FAddD (xt, x, y)
  | FSubD (xt, x, y)
  | FMulD (xt, x, y)
  | FDivD (xt, x, y) ->
      erase_zero [ xt; (x, Type.Float); (y, Type.Float) ]
  | StDF (xt, x, y, Asm.C _) ->
      erase_zero [ xt; (x, Type.Float); (y, Type.Int) ]
  | St (xt, x, y, Asm.V z) ->
      erase_zero [ xt; (x, Type.Int); (y, Type.Int); (z, Type.Int) ]
  | StDF (xt, x, y, Asm.V z) ->
      erase_zero [ xt; (x, Type.Float); (y, Type.Int); (z, Type.Int) ]
  | CallCls (xt, name, args, fargs) ->
      erase_zero
        ([ xt ]
        @ List.map (fun x -> (x, Type.Int)) args
        @ List.map (fun x -> (x, Type.Float)) fargs)
  | CallDir (xt, Id.L name, args, fargs) ->
      erase_zero
        ([ xt ]
        @ List.map (fun x -> (x, Type.Int)) args
        @ List.map (fun x -> (x, Type.Float)) fargs)

let add_S x y env =
  let e = if env_exists x env then env_find x env else [] in
  let e = set_add y e in
  env_add x e env

let get_type x = env_find x !varenv

let get_degree x = env_find x !degree

let get_adj_list x = env_find x !adj_list

let adjacent n =
  set_diff (get_adj_list n) (set_union !select_stack !coalesced_nodes)

let get_move_list x = env_find x !move_list

let get_color x = env_find x !color

let push n = select_stack := n :: !select_stack

let pop () =
  if !select_stack = [] then assert false
  else
    let ans = List.hd !select_stack in
    select_stack := List.tl !select_stack;
    ans

let rm_some = function Some x -> x | None -> assert false

let make_edge u v = u ^ "-" ^ v

let mem_edges u v = set_exist (make_edge u v) !adj_set

let set_varenv fund =
  varenv := [];
  List.iter
    (fun (_, blk) ->
      List.iter
        (fun (_, instr) ->
          let conv t =
            if t = Type.Float || t = Type.Unit then t else Type.Int
          in
          varenv :=
            List.fold_left
              (fun env (x, y) -> env_add x (conv y) env)
              !varenv (get_tp instr))
        blk.instrs)
    fund.blocks;
  List.iter (fun x -> varenv := env_add x Type.Int !varenv) (fund.args @ allregs);
  List.iter
    (fun x -> varenv := env_add x Type.Float !varenv)
    (fund.fargs @ allfregs);
  varenv := env_add "%dummy" Type.Unit !varenv

let set_all_moves fund =
  all_moves := [];
  List.iter
    (fun (_, blk) ->
      List.iter
        (fun (_, instr) ->
          match instr.instr with
          | Mov (dest, src) ->
              if
                fst dest <> "zero"
                && fst dest <> "fzero"
                && src <> "zero" && src <> "fzero"
              then
                all_moves :=
                  env_add instr.id (blk.id, instr.id, dest, src) !all_moves
              else ()
          | FMovD (dest, src) ->
              if
                fst dest <> "zero"
                && fst dest <> "fzero"
                && src <> "zero" && src <> "fzero"
              then
                all_moves :=
                  env_add instr.id (blk.id, instr.id, dest, src) !all_moves
              else ()
          | _ -> ())
        blk.instrs)
    fund.blocks

let get_K n =
  if get_type n = Type.Float then List.length allfregs else List.length allregs

let set_call_nodes_in_instr fund instr livein liveout =
  match instr.instr with
  | CallDir ((x, _), Id.L name, args, fargs) ->
      ret_nodes := set_add x !ret_nodes;
      arg_nodes := set_union (args @ fargs) !arg_nodes;
      striding_nodes := set_union (set_inter livein liveout) !striding_nodes;
      if Id.L name = fund.name then is_loop := true
  | _ -> ()

let add_edge u v =
  let t1 = get_type u and t2 = get_type v in
  let uv = make_edge u v and vu = make_edge v u in
  if t1 = t2 && u <> v && not (set_exist uv !adj_set) then (
    adj_set := set_add uv !adj_set;
    adj_set := set_add vu !adj_set;
    if not (set_exist u !precolored) then (
      adj_list := add_S u v !adj_list;
      degree := env_add u (1 + get_degree u) !degree);
    if not (set_exist v !precolored) then (
      adj_list := add_S v u !adj_list;
      degree := env_add v (1 + get_degree v) !degree))

let node_moves n =
  set_inter (get_move_list n) (set_union !active_moves !worklist_moves)

let move_related n = not ([] = node_moves n)

let enable_moves nodes =
  List.iter
    (fun n ->
      List.iter
        (fun m ->
          if set_exist m !active_moves then (
            active_moves := set_remove m !active_moves;
            worklist_moves := set_add m !worklist_moves))
        (node_moves n))
    nodes

let set_degree n m = degree := env_add m n (env_remove m !degree)

let decrement_degree m =
  let d = get_degree m in
  set_degree (d - 1) m;
  if d <= get_K m then (
    enable_moves (set_add m (adjacent m));
    spill_worklist := set_remove m !spill_worklist;
    if move_related m then freeze_worklist := set_add m !freeze_worklist
    else simplify_worklist := set_add m !simplify_worklist)

let add_worklist u =
  if
    (not (set_exist u !precolored))
    && (not (move_related u))
    && get_degree u < get_K u
  then (
    freeze_worklist := set_remove u !freeze_worklist;
    simplify_worklist := set_add u !simplify_worklist)

let ok t r = get_degree t < get_K t || set_exist t !precolored || mem_edges t r

let conservative nodes =
  let k = ref 0 in
  List.iter (fun n -> if get_degree n >= get_K n then k := !k + 1) nodes;
  [] = nodes || !k < get_K (set_min nodes)

let rec get_alias n =
  if set_exist n !coalesced_nodes then get_alias (env_find n !alias) else n

let combine u v =
  if set_exist v !freeze_worklist then
    freeze_worklist := set_remove v !freeze_worklist
  else spill_worklist := set_remove v !spill_worklist;
  coalesced_nodes := set_add v !coalesced_nodes;
  alias := env_add v u !alias;
  move_list :=
    env_add u (set_union (get_move_list u) (get_move_list v)) !move_list;
  enable_moves [ v ];
  List.iter
    (fun t ->
      add_edge t u;
      decrement_degree t)
    (adjacent v);
  if get_degree u >= get_K u && set_exist u !freeze_worklist then (
    freeze_worklist := set_remove u !freeze_worklist;
    spill_worklist := set_add u !spill_worklist)

let freeze_moves u =
  List.iter
    (fun m ->
      let _, _, (y, _), x =
        try env_find m !all_moves
        with Not_found ->
          Printf.eprintf "%s" m;
          assert false
      in
      let v = if get_alias y = get_alias u then get_alias x else get_alias y in
      active_moves := set_remove m !active_moves;
      frozen_moves := set_add m !frozen_moves;
      if
        (not (set_exist v !precolored))
        && [] = node_moves v
        && get_degree v < get_K v
      then (
        freeze_worklist := set_remove v !freeze_worklist;
        simplify_worklist := set_add v !simplify_worklist))
    (node_moves u)

let select_spill_node fundef =
  let x, hx =
    List.fold_right
      (fun x (m, hm) ->
        let cost =
          List.length (Block.get_use_place x @ Block.get_def_place x)
        in
        let deg = get_degree x in
        let hx = float_of_int cost /. float_of_int deg in
        if hx < hm then (x, hx) else (m, hm))
      !spill_worklist ("", 10000000.0)
  in
  x

let insert_restore fund (block_id, instr_id) x =
  let blk = env_find block_id fund.blocks in
  let instr = env_find instr_id blk.instrs in
  let t = get_type x in
  let id = Block.gen_instr_id () in
  let new_temp = Id.genid x in
  new_temps := set_add new_temp !new_temps;
  let new_instr =
    {
      id;
      block = blk.id;
      instr = Restore ((new_temp, t), x);
      predId = instr.predId;
      nextId = instr.id;
      liveIn = [];
      liveOut = [];
    }
  in
  instr.predId <- id;
  (if new_instr.predId = "" then blk.head <- id
  else
    let pred = env_find new_instr.predId blk.instrs in
    pred.nextId <- id);
  blk.instrs <- env_add id new_instr blk.instrs;
  instr.instr <- Block.replace instr x new_temp

let create_instr id blockid instr predId nextId liveIn liveOut =
  { id; block = blockid; instr; predId; nextId; liveIn; liveOut }

let insert_save_head fund x =
  let block = env_find fund.head fund.blocks in
  let instr = env_find block.head block.instrs in
  let id = Block.gen_instr_id () in
  new_temps := set_add x !new_temps;
  let new_instr =
    create_instr id block.id
      (Save (("%dummy", Type.Unit), x, x))
      "" instr.id [] []
  in
  instr.predId <- id;
  block.head <- id;
  block.instrs <- env_add id new_instr block.instrs

let insert_save_bunki fund block instr x new_temp =
  let get_next b = env_find (List.hd b.nextIds) fund.blocks in
  let rec find_insert_point b cnt =
    let next_len = List.length b.nextIds in
    let next = get_next b in
    if next_len >= 2 then find_insert_point next (cnt + 1)
    else if cnt > 1 then find_insert_point next (cnt - 1)
    else next
  in
  let next = get_next block in
  let target_blk = find_insert_point next 1 in
  let target_instr =
    if [] = target_blk.instrs then None
    else Some (env_find target_blk.head target_blk.instrs)
  in
  let id = Block.gen_instr_id () in
  new_temps := set_add new_temp !new_temps;
  let new_instr =
    create_instr id target_blk.id
      (Save (("%dummy", Type.Unit), new_temp, x))
      ""
      (if target_instr = None then "" else (rm_some target_instr).id)
      [] []
  in
  if target_instr <> None then (rm_some target_instr).predId <- id
  else target_blk.tail <- id;
  target_blk.head <- id;
  target_blk.instrs <- env_add id new_instr target_blk.instrs;
  instr.instr <- Block.replace instr x new_temp;
  Format.eprintf "INSERT %s from (%s, %s) to (%s, %s)\n" id block.id instr.id
    target_blk.id
    (if target_instr = None then "" else (rm_some target_instr).id)

let insert_save fund (block_id, instr_id) x new_temp =
  let block = env_find block_id fund.blocks in
  let instr = env_find instr_id block.instrs in
  if instr.nextId = "" then
    if List.length block.nextIds >= 2 then
      insert_save_bunki fund block instr x new_temp
    else instr.instr <- Block.replace instr x new_temp
  else
    let id = Block.gen_instr_id () in
    let a = Id.genid x in
    new_temps := set_add a !new_temps;
    let new_instr =
      create_instr id block.id
        (Save (("%dummy", Type.Unit), a, x))
        instr.id instr.nextId [] []
    in
    instr.nextId <- id;
    (if new_instr.nextId = "" then block.tail <- id
    else
      let next = env_find new_instr.nextId block.instrs in
      next.predId <- id);
    block.instrs <- env_add id new_instr block.instrs;
    instr.instr <- Block.replace instr x a;
    Format.eprintf "Insert %s from (%s, %s) @." id block.id instr.id

let initialize is_first fundef =
  precolored := allregs @ allfregs;
  set_varenv fundef;
  simplify_worklist := [];
  freeze_worklist := [];
  spill_worklist := [];
  select_stack := [];
  coalesced_moves := [];
  constrained_moves := [];
  frozen_moves := [];
  worklist_moves := [];
  active_moves := [];
  move_list := List.fold_right (fun (x, _) env -> env_add x [] env) !varenv [];
  set_all_moves fundef;
  alias := [];
  adj_set := [];
  adj_list := List.fold_right (fun (x, _) env -> env_add x [] env) !varenv [];
  degree := List.fold_right (fun (x, _) env -> env_add x 0 env) !varenv [];
  color := List.fold_right (fun x env -> env_add x x env) !precolored [];
  color := env_add "zero" "zero" (env_add "fzero" "fzero" !color);

  List.iter (fun x -> List.iter (fun y -> add_edge x y) fundef.args) fundef.args;
  List.iter
    (fun x -> List.iter (fun y -> add_edge x y) fundef.fargs)
    fundef.fargs;
  ret_nodes := [];
  arg_nodes := [];
  striding_nodes := [];
  is_loop := false;
  if is_first then (
    colorenv := [];
    initial :=
      List.fold_right
        (fun (x, t) env ->
          if (not (set_exist x !precolored)) && t <> Type.Unit then
            set_add x env
          else env)
        !varenv [];
    colored_nodes := [];
    spilled_nodes := [];
    coalesced_nodes := [];
    new_temps := [];
    spill_cnt := 0);
  Printf.eprintf "\n<%s> spill %d\n" (Id.get_name fundef.name) !spill_cnt

let is_move_instruction instr =
  match instr.instr with
  | Mov ((x, _), y) -> if x = "zero" || y = "zero" then false else true
  | FMovD ((x, _), y) -> if x = "fzero" || y = "fzero" then false else true
  | _ -> false

let build (fund : Block.fundef) =
  List.iter
    (fun (_, block) ->
      let live = ref block.liveOut in
      let rec iter instr_id =
        let liveout = !live in
        let instr =
          if instr_id = "" then None else Some (env_find instr_id block.instrs)
        in
        let df, us =
          if instr_id = "" then ([], fund.args @ fund.fargs)
          else Block.def_use (rm_some instr)
        in
        if instr_id <> "" && is_move_instruction (rm_some instr) then (
          let instr = rm_some instr in
          live := set_diff !live us;
          List.iter
            (fun n -> move_list := add_S n instr.id !move_list)
            (set_union df us);
          worklist_moves := set_add instr.id !worklist_moves);
        live := set_union !live df;
        List.iter (fun d -> List.iter (fun l -> add_edge l d) !live) df;
        live := set_union us (set_diff !live df);
        let livein = !live in
        if instr_id <> "" then
          set_call_nodes_in_instr fund (rm_some instr) livein liveout;
        if instr_id <> "" then iter (rm_some instr).predId
      in
      if not ([] = block.instrs) then iter block.tail)
    fund.blocks

let make_worklist fund =
  List.iter
    (fun n ->
      if get_degree n >= get_K n then
        spill_worklist := set_add n !spill_worklist
      else if move_related n then freeze_worklist := set_add n !freeze_worklist
      else simplify_worklist := set_add n !simplify_worklist)
    !initial;
  initial := []

let select_simplify_node fund =
  let priority0 = [] in
  let priority1 = set_inter !arg_nodes !simplify_worklist in
  let priority2 = set_inter !ret_nodes !simplify_worklist in
  let priority3 =
    set_diff !simplify_worklist
      (set_union priority0 (set_union priority1 priority2))
  in
  if [] = priority3 then
    if [] = priority2 then
      if [] = priority1 then
        if [] = priority0 then assert false else set_min priority0
      else set_min priority1
    else set_min priority2
  else set_min priority3

let simplify fund =
  let n = select_simplify_node fund in
  simplify_worklist := set_remove n !simplify_worklist;
  push n;
  List.iter (fun m -> decrement_degree m) (adjacent n)

let coalesce fund =
  let m = set_min !worklist_moves in
  let _, _, (y, _), x =
    try env_find m !all_moves
    with Not_found ->
      Format.eprintf "%s" m;
      assert false
  in
  let x = get_alias x and y = get_alias y in

  let u, v = if set_exist y !precolored then (y, x) else (x, y) in
  worklist_moves := set_remove m !worklist_moves;
  if u = v then (
    coalesced_moves := set_add m !coalesced_moves;
    add_worklist u)
  else if set_exist v !precolored || mem_edges u v then (
    constrained_moves := set_add m !constrained_moves;
    add_worklist u;
    add_worklist v)
  else if
    set_exist u !precolored
    && List.fold_right (fun t bl -> bl && ok t u) (adjacent v) true
    || (not (set_exist u !precolored))
       && conservative (set_union (adjacent u) (adjacent v))
  then (
    coalesced_moves := set_add m !coalesced_moves;
    combine u v;
    add_worklist u)
  else active_moves := set_add m !active_moves

let freeze fundef =
  let u = set_min !freeze_worklist in
  freeze_worklist := set_remove u !freeze_worklist;
  simplify_worklist := set_add u !simplify_worklist;
  freeze_moves u

let select_spill fundef =
  let m =
    if !spill_cnt mod 2 = 1 then set_min !spill_worklist
    else select_spill_node fundef
  in
  spill_worklist := set_remove m !spill_worklist;
  simplify_worklist := set_add m !simplify_worklist;
  freeze_moves m

let no_together = ref []

let together = ref []

let set_not_together n li =
  let a = try env_find n !no_together with Not_found -> [] in
  no_together := env_add n (a @ li) !no_together

let set_together n li =
  let a = try env_find n !together with Not_found -> [] in
  together := env_add n (a @ li) !together

let get_arg_regs x =
  try (env_find x !fundata).arg_regs
  with Not_found ->
    Printf.eprintf "Not_found %s\n" x;
    assert false

let get_ret_reg x =
  try (env_find x !fundata).ret_reg
  with Not_found ->
    Printf.eprintf "Not_found %s\n" x;
    assert false

let get_use_regs x =
  try (env_find x !fundata).use_regs
  with Not_found ->
    Printf.printf "\tNotFound %s\n" x;
    allregs @ allfregs

let set_color_env fund =
  no_together := [];
  together := [];
  List.map
    (fun (_, block) ->
      List.map
        (fun (_, instr) ->
          match instr.instr with
          | CallDir ((dest, _), Id.L name, args, fargs)
            when Id.L name <> fund.name ->
              let args_called = get_arg_regs name in
              (* Format.eprintf "%s :" name; *)
              (* Asm.print_regs args_called; *)
              (* Format.eprintf "\n"; *)
              (* Asm.print_regs (args @ fargs); *)
              (* Format.eprintf "\n"; *)
              let uses = set_union (get_use_regs name) args_called in
              List.iter
                (fun n -> set_not_together n uses)
                (set_inter instr.liveIn instr.liveOut);
              List.iter2
                (fun call called ->
                  set_together call [ called ];
                  set_not_together call (set_remove called args_called))
                (args @ fargs) args_called;
              set_together dest [ get_ret_reg name ]
          | _ -> ())
        block.instrs)
    fund.blocks

let rec s_dif s li =
  match li with [] -> s | l :: ls -> s_dif (set_remove l s) ls

let find_color remain =
  if List.length remain > 0 then (
    let r = ref remain in
    let a = ref (!cc mod List.length remain) in
    while !a > 0 do
      r := set_remove (set_min !r) !r;
      a := !a - 1
    done;
    set_min !r)
  else ""

let select_color fund n ok_colors =
  let ans = ref "" in
  if env_exists n !together then
    List.iter
      (fun a ->
        let a = if env_exists a !color then env_find a !color else a in
        if set_exist a ok_colors then (
          ans := a;
          ())
        else ())
      (env_find n !together)
  else if env_exists n !no_together then
    let no = env_find n !no_together in
    let no =
      List.map
        (fun a -> if env_exists a !color then env_find a !color else a)
        no
    in
    let remain = s_dif ok_colors no in
    if not ([] = remain) then ans := find_color remain else ()
  else ();
  if !ans = "" then ans := find_color ok_colors else ();
  !ans

let assign_colors fundef =
  while !select_stack <> [] do
    let n = pop () in
    let ok_colors =
      ref (if get_type n = Type.Float then allfregs else allregs)
    in
    List.iter
      (fun w ->
        if set_exist (get_alias w) (set_union !colored_nodes !precolored) then
          ok_colors := set_remove (get_color (get_alias w)) !ok_colors)
      (get_adj_list n);
    if [] = !ok_colors then (
      Format.eprintf "%s" n;
      spilled_nodes := set_add n !spilled_nodes)
    else (
      colored_nodes := set_add n !colored_nodes;
      let c = select_color fundef n !ok_colors in
      color := env_add n c !color)
  done;
  List.iter
    (fun n -> color := env_add n (get_color (get_alias n)) !color)
    !coalesced_nodes

let rewrite_program fund =
  new_temps := [];
  List.iter
    (fun n ->
      Format.eprintf "spilled %s " n;
      if List.mem n (fund.args @ fund.fargs) then insert_save_head fund n;
      let new_temp = Id.genid n in
      List.iter
        (fun site -> insert_save fund site n new_temp)
        (Block.get_def_place n);
      List.iter (fun site -> insert_restore fund site n) (Block.get_use_place n))
    !spilled_nodes;
  spilled_nodes := [];

  initial := set_union !colored_nodes (set_union !coalesced_nodes !new_temps);
  colored_nodes := [];
  coalesced_nodes := [];
  spill_cnt := 1 + !spill_cnt

type s = { id : Id.t; mutable nextIds : Id.t list; mutable predIds : Id.t list }

let rec blk_analy block current_instr_id us df =
  let current_instr = env_find current_instr_id block.instrs in
  let d, u = Block.def_use current_instr in
  df := set_union d !df;
  us := set_union u (set_diff !us d);
  if current_instr.predId <> "" then blk_analy block current_instr.predId us df
  else (!us, !df)

let inside_block_analysis fund =
  Block.remember_place_of_us_df_init ();
  List.iter
    (fun (_, block) ->
      let us = ref [] in
      let df = ref [] in
      let us, df =
        if [] = block.instrs then ([], []) else blk_analy block block.tail us df
      in
      remember_place_of_us_df block;
      block.useInside <- us;
      block.defInside <- df)
    fund.blocks

let rec find_blocks_from_id ids blocks =
  match ids with
  | [] -> []
  | i :: is -> env_find i blocks :: find_blocks_from_id is blocks

let rec set_liveout vars blocks =
  match blocks with
  | [] -> vars
  | v :: vs -> set_liveout (set_union v.liveIn vars) vs

let rec list_union l1 l2 =
  match l1 with
  | [] -> l2
  | l :: ls ->
      if List.mem l l2 then list_union ls l2 else list_union ls (l :: l2)

let rec list_remove i li =
  match li with
  | [] -> []
  | l :: ls -> if l = i then list_remove i ls else l :: list_remove i ls

let rec list_dif l1 l2 =
  match l2 with
  | [] -> []
  | l :: ls -> if List.mem l l1 then list_dif l1 ls else l :: list_dif l1 ls

let rec get_next_ids current =
  match current with
  | [] -> []
  | c :: cs -> list_union c.nextIds (get_next_ids cs)

let rec removing b1 b2 =
  let rec removing_sub b1 b2 result =
    match b2 with
    | [] -> result
    | b :: bs ->
        if List.mem b b1 then removing_sub b1 bs result
        else removing_sub b1 bs (b :: result)
  in
  removing_sub b1 b2 []

let find_next all_blocks current_blocks =
  let current_block_ids = List.map (fun block -> block.id) current_blocks in
  let rec find_next_sub all_blocks next =
    match all_blocks with
    | [] -> next
    | b :: bs ->
        b.predIds <- list_dif current_block_ids b.predIds;
        if List.length b.predIds > 0 then find_next_sub bs next
        else find_next_sub bs (b :: next)
  in
  find_next_sub all_blocks []

let rec sort_for_liveness fund =
  let blocks = fund.blocks in
  let current =
    ref
      [
        (let block = env_find fund.head blocks in
         { id = block.id; nextIds = block.nextIds; predIds = block.predIds });
      ]
  in
  let result =
    ref
      [
        (let a = env_find fund.head blocks in
         a.id);
      ]
  in
  let a = List.fold_right (fun (_, x) env -> x :: env) blocks [] in
  let remain =
    ref
      (List.map
         (fun (block : block) ->
           { id = block.id; nextIds = block.nextIds; predIds = block.predIds })
         a)
  in
  remain := removing !current !remain;
  while List.length !remain > 0 do
    let next = find_next !remain !current in
    result := !result @ List.map (fun a -> a.id) next;
    remain := removing next !remain;
    current := next
  done;
  List.rev !result

let rec live_analysis fund =
  inside_block_analysis fund;

  List.iter
    (fun (_, block) ->
      block.liveIn <- [];
      block.liveOut <- [])
    fund.blocks;

  let block_id_list = sort_for_liveness fund in

  let finish = ref false in
  while not !finish do
    finish := true;
    List.iter
      (fun block_id ->
        let block = env_find block_id fund.blocks in
        let livein = block.liveIn in
        let liveout = block.liveOut in
        let next_blocks = find_blocks_from_id block.nextIds fund.blocks in

        block.liveIn <-
          set_union block.useInside (set_diff block.liveOut block.defInside);
        block.liveOut <- set_liveout [] next_blocks;

        if livein <> block.liveIn || liveout <> block.liveOut then
          finish := false
        else ())
      block_id_list
  done

let rec main is_first (fund : Block.fundef) =
  let (Id.L name) = fund.name in
  Format.eprintf "%s start@." name;
  cc := !cc + 1;
  initialize is_first fund;
  set_color_env fund;
  live_analysis fund;
  build fund;
  make_worklist fund;
  while
    not
      ([] = !simplify_worklist && [] = !worklist_moves && [] = !freeze_worklist
     && [] = !spill_worklist)
  do
    if not ([] = !simplify_worklist) then simplify fund
    else if not ([] = !worklist_moves) then coalesce fund
    else if not ([] = !freeze_worklist) then freeze fund
    else if not ([] = !spill_worklist) then select_spill fund
  done;
  assign_colors fund;
  if not ([] = !spilled_nodes) then (
    rewrite_program fund;
    main false fund)
  else (
    (try
       let data = env_find name !fundata in
       let args =
         List.map
           (fun x -> try env_find x !color with Not_found -> "a0")
           (fund.args @ fund.fargs)
       in
       let data = { data with arg_regs = args } in
       fundata := env_add name data !fundata
     with Not_found -> assert (name = "min_caml_start"));
    colorenv := env_add name !color !colorenv)

let current_pos = ref ""

exception NoReg of Id.t * Type.t

let add x r regenv =
  if Asm.is_reg x then (
    assert (x = r);
    regenv)
  else env_add x r regenv

let find x t regenv =
  if Asm.is_reg x then x
  else try env_find x regenv with Not_found -> raise (NoReg (x, t))

let find' x' regenv =
  match x' with Asm.V x -> Asm.V (find x Type.Int regenv) | c -> c

let rec concat e1 xt e2 =
  match e1 with
  | Asm.Ans exp -> Asm.Let (xt, exp, e2)
  | Asm.Let (yt, exp, e1') -> Asm.Let (yt, exp, concat e1' xt e2)

let rec g dest cont regenv = function
  | Asm.Ans exp -> g'_and_restore dest cont regenv exp
  | Asm.Let (((x, t) as xt), exp, e) ->
      let cont' = concat e dest cont in
      let e1', regenv1 = g'_and_restore xt cont' regenv exp in
      let r =
        if Asm.is_reg x then x
        else if t = Type.Unit then "%dummy"
        else env_find x !color
      in
      let e2', regenv2 = g dest cont (add x r regenv1) e in
      (concat e1' (r, t) e2', regenv2)

and g'_and_restore dest cont regenv exp =
  try g' dest cont regenv exp
  with NoReg (x, t) ->
    g dest cont regenv (Asm.Let ((x, t), Asm.Restore x, Asm.Ans exp))

and g' dest cont regenv = function
  | ( Asm.Nop | Asm.Set _ | Asm.SetF _ | Asm.SetL _ | Asm.Comment _
    | Asm.Restore _ ) as exp ->
      (Asm.Ans exp, regenv)
  | Asm.Mov x -> (Asm.Ans (Asm.Mov (find x Type.Int regenv)), regenv)
  | Asm.Neg x -> (Asm.Ans (Asm.Neg (find x Type.Int regenv)), regenv)
  | Asm.Add (x, y') ->
      (Asm.Ans (Asm.Add (find x Type.Int regenv, find' y' regenv)), regenv)
  | Asm.Sub (x, y') ->
      (Asm.Ans (Asm.Sub (find x Type.Int regenv, find' y' regenv)), regenv)
  | Asm.SLL (x, y') ->
      (Asm.Ans (Asm.SLL (find x Type.Int regenv, find' y' regenv)), regenv)
  | Asm.SRL (x, y') ->
      (Asm.Ans (Asm.SRL (find x Type.Int regenv, find' y' regenv)), regenv)
  | Asm.Ld (x, y') ->
      (Asm.Ans (Asm.Ld (find x Type.Int regenv, find' y' regenv)), regenv)
  | Asm.St (x, y, z') ->
      ( Asm.Ans
          (Asm.St
             (find x Type.Int regenv, find y Type.Int regenv, find' z' regenv)),
        regenv )
  | Asm.FMovD x -> (Asm.Ans (Asm.FMovD (find x Type.Float regenv)), regenv)
  | Asm.FNegD x -> (Asm.Ans (Asm.FNegD (find x Type.Float regenv)), regenv)
  | Asm.FAddD (x, y) ->
      ( Asm.Ans (Asm.FAddD (find x Type.Float regenv, find y Type.Float regenv)),
        regenv )
  | Asm.FSubD (x, y) ->
      ( Asm.Ans (Asm.FSubD (find x Type.Float regenv, find y Type.Float regenv)),
        regenv )
  | Asm.FMulD (x, y) ->
      ( Asm.Ans (Asm.FMulD (find x Type.Float regenv, find y Type.Float regenv)),
        regenv )
  | Asm.FDivD (x, y) ->
      ( Asm.Ans (Asm.FDivD (find x Type.Float regenv, find y Type.Float regenv)),
        regenv )
  | Asm.LdDF (x, y') ->
      (Asm.Ans (Asm.LdDF (find x Type.Int regenv, find' y' regenv)), regenv)
  | Asm.StDF (x, y, z') ->
      ( Asm.Ans
          (Asm.StDF
             (find x Type.Float regenv, find y Type.Int regenv, find' z' regenv)),
        regenv )
  | Asm.IfEq (x, y', e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          Asm.IfEq (find x Type.Int regenv, find' y' regenv, e1', e2'))
        e1 e2
  | Asm.IfLE (x, y', e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          Asm.IfLE (find x Type.Int regenv, find' y' regenv, e1', e2'))
        e1 e2
  | Asm.IfGE (x, y', e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          Asm.IfGE (find x Type.Int regenv, find' y' regenv, e1', e2'))
        e1 e2
  | Asm.IfFEq (x, y, e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          Asm.IfFEq
            (find x Type.Float regenv, find y Type.Float regenv, e1', e2'))
        e1 e2
  | Asm.IfFLE (x, y, e1, e2) as exp ->
      g'_if dest cont regenv exp
        (fun e1' e2' ->
          Asm.IfFLE
            (find x Type.Float regenv, find y Type.Float regenv, e1', e2'))
        e1 e2
  | Asm.CallCls (x, ys, zs) as exp ->
      g'_call x dest cont regenv exp
        (fun ys zs -> Asm.CallCls (find x Type.Int regenv, ys, zs))
        ys zs
  | Asm.CallDir (Id.L x, ys, zs) as exp ->
      g'_call x dest cont regenv exp
        (fun ys zs -> Asm.CallDir (Id.L x, ys, zs))
        ys zs
  | Asm.Save (x, y) -> (Asm.Ans (Asm.Save (find x Type.Unit regenv, y)), regenv)
  | Asm.Fiszero x -> (Asm.Ans (Asm.Fiszero (find x Type.Float regenv)), regenv)
  | Asm.Fispos x -> (Asm.Ans (Asm.Fispos (find x Type.Float regenv)), regenv)
  | Asm.Fisneg x -> (Asm.Ans (Asm.Fisneg (find x Type.Float regenv)), regenv)
  | Asm.Fneg x -> (Asm.Ans (Asm.Fneg (find x Type.Float regenv)), regenv)
  | Asm.Fless (x, y) ->
      ( Asm.Ans (Asm.Fless (find x Type.Float regenv, find y Type.Float regenv)),
        regenv )
  | Asm.IntOfFloat x ->
      (Asm.Ans (Asm.IntOfFloat (find x Type.Float regenv)), regenv)
  | Asm.FloatOfInt x ->
      (Asm.Ans (Asm.FloatOfInt (find x Type.Int regenv)), regenv)
  | Asm.Sqrt x -> (Asm.Ans (Asm.Sqrt (find x Type.Float regenv)), regenv)
  | Asm.Fsqr x -> (Asm.Ans (Asm.Fsqr (find x Type.Float regenv)), regenv)

and g'_if dest cont regenv exp constr e1 e2 =
  let e1', regenv1 = g dest cont regenv e1 in
  let e2', regenv2 = g dest cont regenv e2 in
  let regenv' =
    List.fold_left
      (fun regenv' x ->
        try
          if Asm.is_reg x then regenv'
          else
            let r1 = env_find x regenv1 in
            let r2 = env_find x regenv2 in
            if r1 <> r2 then regenv' else env_add x r1 regenv'
        with Not_found -> regenv')
      [] (Asm.fv cont)
  in
  let regenv' = env_add "zero" "zero" (env_add "fzero" "fzero" regenv') in
  ( List.fold_left
      (fun e x ->
        if x = fst dest || (not (env_exists x regenv)) || env_exists x regenv'
        then e
        else Asm.seq (Asm.Save (env_find x regenv, x), e))
      (Asm.Ans (constr e1' e2'))
      (Asm.fv cont),
    regenv' )

and g'_call id dest cont regenv exp constr ys zs =
  List.fold_left
    (fun (e, env) x ->
      if x = fst dest || not (env_exists x regenv) then (e, env)
      else if set_exist (env_find x regenv) (get_use_regs id) then
        (Asm.seq (Asm.Save (env_find x regenv, x), e), env)
      else if id = !current_pos then
        (Asm.seq (Asm.Save (env_find x regenv, x), e), env)
      else (e, env_add x (env_find x regenv) env))
    ( Asm.Ans
        (constr
           (List.map (fun y -> find y Type.Int regenv) ys)
           (List.map (fun z -> find z Type.Float regenv) zs)),
      [ ("zero", "zero"); ("fzero", "fzero") ] )
    (Asm.fv cont)

let rec get_use_regs_ id = function
  | Asm.Ans e -> get_use_regs_' id e
  | Asm.Let ((x, _), e, t) ->
      set_add x (set_union (get_use_regs_' id e) (get_use_regs_ id t))

and get_use_regs_' id = function
  | Asm.IfEq (_, _, e1, e2) | IfLE (_, _, e1, e2) | IfGE (_, _, e1, e2) ->
      set_union (get_use_regs_ id e1) (get_use_regs_ id e2)
  | Asm.CallDir (Id.L x, ys, zs) when Asm.is_reg x -> assert false
  | Asm.CallDir (Id.L x, ys, zs) when x = id -> []
  | Asm.CallDir (Id.L x, ys, zs) -> get_use_regs x
  | Asm.CallCls (x, ys, zs) when Asm.is_reg x && x <> Asm.reg_cl ->
      allregs @ allfregs
  | Asm.CallCls (x, ys, zs) when x = Asm.reg_cl || x = id -> []
  | Asm.CallCls (x, ys, zs) -> get_use_regs x
  | Asm.SetL (Id.L x) when String.sub x 0 2 = "l." -> []
  | Asm.SetL (Id.L x) when x = id -> []
  | _ -> []

let h
    {
      Asm.name = Id.L x;
      Asm.args = ys;
      Asm.fargs = zs;
      Asm.body = e;
      Asm.ret = t;
    } =
  let data =
    if env_exists x !fundata then env_find x !fundata else assert false
  in
  current_pos := x;

  let regenv = [ ("fzero", "fzero"); ("zero", "zero") ] in
  let regenv =
    List.fold_left2
      (fun env x r -> env_add x r env)
      regenv (ys @ zs) data.arg_regs
  in
  let cont =
    Asm.Ans
      (if t = Type.Float then Asm.FMovD data.ret_reg else Asm.Mov data.ret_reg)
  in

  let e', _ = g (data.ret_reg, t) cont regenv e in

  fundata := env_add x data !fundata;
  let env =
    set_union data.arg_regs (set_add data.ret_reg (get_use_regs_ x e'))
  in
  let env = List.filter Asm.is_reg env in
  let env = set_union [ Asm.reg_sw; Asm.reg_fsw ] env in
  let env = set_remove "zero" (set_remove "fzero" env) in
  let data = { data with use_regs = env } in
  fundata := env_add x data !fundata;

  {
    Asm.name = Id.L x;
    Asm.args = List.filter (fun x -> List.mem x allregs) data.arg_regs;
    Asm.fargs = List.filter (fun x -> List.mem x allfregs) data.arg_regs;
    Asm.body = e';
    Asm.ret = t;
  }

let rec blk2asm_g fund b_id =
  let block = env_find b_id fund.blocks in
  blk2asm_g' fund block block.head

and blk2asm_g' fund block instr_id =
  let instr = env_find instr_id block.instrs in
  let dest, exp, (n_block : Block.block) = create_asm fund block instr.instr in
  if instr.nextId <> "" then
    let e, n_block = blk2asm_g' fund block instr.nextId in
    (Asm.Let (dest, exp, e), n_block)
  else if List.length n_block.nextIds <= 0 then (Asm.Ans exp, n_block)
  else
    let next_id = List.hd n_block.nextIds in
    let next_block = env_find next_id fund.blocks in
    if List.length block.nextIds = 1 then (Asm.Ans exp, n_block)
    else if [] = next_block.instrs then (Asm.Ans exp, next_block)
    else
      let e, n_block = blk2asm_g' fund next_block next_block.head in
      (Asm.Let (dest, exp, e), n_block)

and create_asm f block = function
  | Block.Nop xt -> (xt, Asm.Nop, block)
  | Block.Set (xt, x) -> (xt, Asm.Set x, block)
  | Block.SetF (xt, x) -> (xt, Asm.SetF x, block)
  | Block.SetL (xt, Id.L x) -> (xt, Asm.SetL (Id.L x), block)
  | Block.Mov (xt, x) -> (xt, Asm.Mov x, block)
  | Block.Neg (xt, x) -> (xt, Asm.Neg x, block)
  | Block.Add (xt, x, y') -> (xt, Asm.Add (x, y'), block)
  | Block.Sub (xt, x, y') -> (xt, Asm.Sub (x, y'), block)
  | Block.SLL (xt, x, y') -> (xt, Asm.SLL (x, y'), block)
  | Block.SRL (xt, x, y') -> (xt, Asm.SRL (x, y'), block)
  | Block.Ld (xt, x, y') -> (xt, Asm.Ld (x, y'), block)
  | Block.St (xt, x, y, z') -> (xt, Asm.St (x, y, z'), block)
  | Block.FMovD (xt, x) -> (xt, Asm.FMovD x, block)
  | Block.FNegD (xt, x) -> (xt, Asm.FNegD x, block)
  | Block.FAddD (xt, x, y) -> (xt, Asm.FAddD (x, y), block)
  | Block.FSubD (xt, x, y) -> (xt, Asm.FSubD (x, y), block)
  | Block.FMulD (xt, x, y) -> (xt, Asm.FMulD (x, y), block)
  | Block.FDivD (xt, x, y) -> (xt, Asm.FDivD (x, y), block)
  | Block.LdDF (xt, x, y') -> (xt, Asm.LdDF (x, y'), block)
  | Block.StDF (xt, x, y, z') -> (xt, Asm.StDF (x, y, z'), block)
  | Block.IfEq (xt, x, y', b1, b2) ->
      let e1, next_block1 = blk2asm_g f b1 in
      let e2, next_block2 = blk2asm_g f b2 in
      (xt, Asm.IfEq (x, y', e1, e2), next_block1)
  | Block.IfLE (xt, x, y', b1, b2) ->
      let e1, next_block1 = blk2asm_g f b1 in
      let e2, next_block2 = blk2asm_g f b2 in
      (xt, Asm.IfLE (x, y', e1, e2), next_block1)
  | Block.IfGE (xt, x, y', b1, b2) ->
      let e1, next_block1 = blk2asm_g f b1 in
      let e2, next_block2 = blk2asm_g f b2 in
      (xt, Asm.IfGE (x, y', e1, e2), next_block1)
  | Block.IfFEq (xt, x, y', b1, b2) ->
      let e1, next_block1 = blk2asm_g f b1 in
      let e2, next_block2 = blk2asm_g f b2 in
      (xt, Asm.IfFEq (x, y', e1, e2), next_block1)
  | Block.IfFLE (xt, x, y', b1, b2) ->
      let e1, next_block1 = blk2asm_g f b1 in
      let e2, next_block2 = blk2asm_g f b2 in
      (xt, Asm.IfFLE (x, y', e1, e2), next_block1)
  | Block.CallCls (xt, name, args, fargs) ->
      (xt, Asm.CallCls (name, args, fargs), block)
  | Block.CallDir (xt, Id.L name, args, fargs) ->
      (xt, Asm.CallDir (Id.L name, args, fargs), block)
  | Block.Save (xt, x, y) -> (xt, Asm.Save (x, y), block)
  | Block.Restore (xt, x) -> (xt, Asm.Restore x, block)
  | Block.Fiszero (xt, x) -> (xt, Asm.Fiszero x, block)
  | Block.Fispos (xt, x) -> (xt, Asm.Fispos x, block)
  | Block.Fisneg (xt, x) -> (xt, Asm.Fisneg x, block)
  | Block.Fneg (xt, x) -> (xt, Asm.Fneg x, block)
  | Block.Fless (xt, x, y) -> (xt, Asm.Fless (x, y), block)
  | Block.IntOfFloat (xt, x) -> (xt, Asm.IntOfFloat x, block)
  | Block.FloatOfInt (xt, x) -> (xt, Asm.FloatOfInt x, block)
  | Block.Sqrt (xt, x) -> (xt, Asm.Sqrt x, block)
  | Block.Fsqr (xt, x) -> (xt, Asm.Fsqr x, block)

let blk2asm_h fundef =
  {
    Asm.name = fundef.name;
    Asm.args = fundef.args;
    Asm.fargs = fundef.fargs;
    Asm.body = fst (blk2asm_g fundef fundef.head);
    Asm.ret = fundef.ret;
  }

let blk2asm (Block.Prog (data, fundefs, fund)) =
  Asm.Prog (data, List.map blk2asm_h fundefs, (blk2asm_h fund).body)

let f (Block.Prog (data, fundefs, main_fun)) =
  Format.eprintf
    "register allocation: may take some time (up to a few minutes, depending \
     on the size of functions)@.";
  let fundefs' =
    List.map
      (fun fundef ->
        main true fundef;
        h (blk2asm_h fundef))
      fundefs
  in
  main true main_fun;
  let e = (blk2asm_h main_fun).body in
  let e', regenv' =
    g
      (Id.gentmp Type.Unit, Type.Unit)
      (Ans Nop)
      [ ("zero", "zero"); ("fzero", "fzero") ]
      e
  in

  let ans = Asm.Prog (data, fundefs', e') in
  ans
