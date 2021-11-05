open Ds

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
    "a0";
    "a1";
    "s0";
    "s1";
    "s2";
    "s3";
    "s4";
    "s5";
    "s6";
    "s7";
    "s8";
    "s9";
    "s10";
    "s11";
    "s12";
    "s13";
    "s14";
    "s15";
    "s16";
    "s17";
  |]

let fregs = Array.init 16 (fun i -> Printf.sprintf "%%f%d" (i * 2))

let allregs = Array.to_list regs

let allfregs = Array.to_list fregs

let reg_cl = regs.(Array.length regs - 2)

let reg_sw = regs.(Array.length regs - 1)

let reg_fsw = fregs.(Array.length fregs - 1)

let reg_sp = "sp"

let reg_hp = "hp"

let reg_ra = "a0"

let is_reg x =
  Array.exists (fun a -> a = x) regs
  || Array.exists (fun a -> a = x) fregs
  || x = reg_sp || x = reg_hp || x = reg_ra

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
