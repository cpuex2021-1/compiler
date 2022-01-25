open Ds
open Asm

type t = (Id.t * Type.t) * Asm.exp

type gid = string

type iid = string

type insn = {
  mutable id : Id.t;
  mutable block : Id.t;
  mutable insn : t;
  mutable liveIn : list;
  mutable liveOut : list;
  mutable nextId : Id.t;
  mutable predId : Id.t;
}

type block = {
  mutable id : Id.t;
  mutable func : Id.l;
  mutable insns : insn list;
  mutable head : Id.t;
  mutable tail : Id.t;
  mutable predIds : Id.t list;
  mutable nextIds : Id.t list;
  mutable liveIn : list;
  mutable liveOut : list;
  mutable useInside : list;
  mutable defInside : list;
}

let dfa (insns: Asm.t) = 

let f' fundef =
  let name = fundef.name in
  let args = fundef.args in
  let fargs = fundef.fargs in
  let body = fundef.body in
  let body' = dfa body in 
  let ret = fundef.ret in
  { name : name; args : args; fargs : fargs; body : body'; ret : ret }

let f (Asm.Prog (data, fundefs, e)) =
  Asm.Prog (data, List.map f' fundefs, dfa e)

let rec opt e n  =
  if n = 0 then e
  else
    let e' = constfold e in
    let e' = elim e' in
    let e' = peephole e' in
    let e' = constreg e' in
    if e = e' then e else f e' (n - 1) th