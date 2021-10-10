let rec env_exists x env =
  match env with
  | (a, b) :: xs -> if a = x then true else env_exists x xs
  | [] -> false

let rec env_find x env =
  match env with
  | (a, b) :: xs -> if a = x then b else env_find x xs
  | [] -> raise Not_found

let rec env_replace x t env =
  match env with
  | (a, b) :: xs -> if a = x then (a, t) :: xs else (a, b) :: env_replace x t xs
  | [] -> raise Not_found

let rec env_add x t env =
  if env_exists x env then
    if env_find x env = t then env else env_replace x t env
  else env @ [ (x, t) ]

let rec env_map f env =
  match env with (a, b) :: xs -> (a, f b) :: env_map f xs | [] -> []

let add_list xys env =
  List.fold_left (fun env (x, y) -> env_add x y env) env xys

let add_list2 xs ys env =
  List.fold_left2 (fun env x y -> env_add x y env) env xs ys

let rec set_exist x s =
  match s with f :: r -> if f = x then true else set_exist x r | [] -> false

let set_add x s = if set_exist x s then s else s @ [ x ]

let set_union s1 s2 = s1 @ s2

let rec set_remove x s =
  match s with
  | f :: r -> if x = f then set_remove x r else f :: set_remove x r
  | [] -> []

let rec set_diff s1 s2 =
  match s1 with
  | f :: r -> if set_exist f s2 then set_diff r s2 else f :: set_diff r s2
  | [] -> []

type id_or_imm = V of Id.t | C of int

type t = Ans of exp | Let of (Id.t * Type.t) * exp * t

and exp =
  | Nop
  | Set of int
  | SetL of Id.l
  | Mov of Id.t
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | SLL of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm
  | St of Id.t * Id.t * id_or_imm
  | FMovD of Id.t
  | FNegD of Id.t
  | FAddD of Id.t * Id.t
  | FSubD of Id.t * Id.t
  | FMulD of Id.t * Id.t
  | FDivD of Id.t * Id.t
  | LdDF of Id.t * id_or_imm
  | StDF of Id.t * Id.t * id_or_imm
  | Comment of string
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t

type fundef = {
  name : Id.l;
  args : Id.t list;
  fargs : Id.t list;
  body : t;
  ret : Type.t;
}

type prog = Prog of (Id.l * float) list * fundef list * t

let fletd (x, e1, e2) = Let ((x, Type.Float), e1, e2)

let seq (e1, e2) = Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs =
  [|
    "%i2";
    "%i3";
    "%i4";
    "%i5";
    "%l0";
    "%l1";
    "%l2";
    "%l3";
    "%l4";
    "%l5";
    "%l6";
    "%l7";
    "%o0";
    "%o1";
    "%o2";
    "%o3";
    "%o4";
    "%o5";
  |]

let fregs = Array.init 16 (fun i -> Printf.sprintf "%%f%d" (i * 2))

let allregs = Array.to_list regs

let allfregs = Array.to_list fregs

let reg_cl = regs.(Array.length regs - 2)

let reg_sw = regs.(Array.length regs - 1)

let reg_fsw = fregs.(Array.length fregs - 1)

let reg_sp = "%i0"

let reg_hp = "%i1"

let reg_ra = "%o7"

let is_reg x = x.[0] = '%'

let co_freg_table =
  let ht = Hashtbl.create 16 in
  for i = 0 to 15 do
    Hashtbl.add ht
      (Printf.sprintf "%%f%d" (i * 2))
      (Printf.sprintf "%%f%d" ((i * 2) + 1))
  done;
  ht

let co_freg freg = Hashtbl.find co_freg_table freg

let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when set_exist x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (set_add x xs) ys

let fv_id_or_imm = function V x -> [ x ] | _ -> []

let rec fv_exp = function
  | Nop | Set _ | SetL _ | Comment _ | Restore _ -> []
  | Mov x | Neg x | FMovD x | FNegD x | Save (x, _) -> [ x ]
  | Add (x, y') | Sub (x, y') | SLL (x, y') | Ld (x, y') | LdDF (x, y') ->
      x :: fv_id_or_imm y'
  | St (x, y, z') | StDF (x, y, z') -> x :: y :: fv_id_or_imm z'
  | FAddD (x, y) | FSubD (x, y) | FMulD (x, y) | FDivD (x, y) -> [ x; y ]
  | IfEq (x, y', e1, e2) | IfLE (x, y', e1, e2) | IfGE (x, y', e1, e2) ->
      x :: fv_id_or_imm y' @ remove_and_uniq [] (fv e1 @ fv e2)
  | IfFEq (x, y, e1, e2) | IfFLE (x, y, e1, e2) ->
      x :: y :: remove_and_uniq [] (fv e1 @ fv e2)
  | CallCls (x, ys, zs) -> x :: ys @ zs
  | CallDir (_, ys, zs) -> ys @ zs

and fv = function
  | Ans exp -> fv_exp exp
  | Let ((x, t), exp, e) -> fv_exp exp @ remove_and_uniq [ x ] (fv e)

let fv e = remove_and_uniq [] (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans exp -> Let (xt, exp, e2)
  | Let (yt, exp, e1') -> Let (yt, exp, concat e1' xt e2)

let align i = if i mod 8 = 0 then i else i + 4
