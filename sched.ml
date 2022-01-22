open Ds
open Virtual

(* type t = (Id.t * Type.t) * Asm.exp *)

(* Asm.Ans -> Asm.Let("ret", ...) *)

(* let reach (p : list t) = p *)

(* let block e = e *)
(* let blocks = list of blocks *)

(* let sched
    { Asm.name = l; Asm.args = xs; Asm.fargs = ys; Asm.body = e; Asm.ret = t } =
  {
    Asm.name = l;
    Asm.args = xs;
    Asm.fargs = ys;
    Asm.body = block e;
    Asm.ret = t;
  }

let f (Asm.Prog (data, fundefs, e)) =
  Asm.Prog (data, List.map sched fundefs, sched e) *)

let f x = x
