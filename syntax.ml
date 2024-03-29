type t =
  | Unit
  (* | Bool of bool *)
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | GlobalTuple of t list
  | GlobalArray of t * t

and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec print_t t indent =
  let indent_next = indent + 2 in
  String.make indent ' '
  ^
  match t with
  | Unit -> "UNIT"
  (* | Bool b -> "BOOL " ^ string_of_bool b *)
  | Int i -> "INT " ^ string_of_int i
  | Float f -> "FLOAT " ^ string_of_float f
  | Not t -> "NOT\n" ^ print_t t indent_next
  | Neg t -> "NEG\n" ^ print_t t indent_next
  | Add (t1, t2) ->
      "ADD\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | Sub (t1, t2) ->
      "SUB\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | Mul (t1, t2) ->
      "MUL\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | Div (t1, t2) ->
      "DIV\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | FNeg t -> "FNEG " ^ print_t t indent_next
  | FAdd (t1, t2) ->
      "FADD\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | FSub (t1, t2) ->
      "FSub\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | FMul (t1, t2) ->
      "FMul\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | FDiv (t1, t2) ->
      "FDiv\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | Eq (t1, t2) ->
      "Eq\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | LE (t1, t2) ->
      "LE\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | If (b, t1, t2) ->
      "IF\n" ^ print_t b indent_next ^ "\n" ^ print_t t1 indent_next ^ "\n"
      ^ print_t t2 indent_next
  | Let ((id, ty), t1, t2) ->
      "LET\n"
      ^ String.make indent_next ' '
      ^ Id.to_string id ^ "\n"
      ^ String.make indent_next ' '
      ^ Type.print ty ^ "\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent
  | Var t -> "VAR " ^ Id.to_string t
  | LetRec (fdef, t) ->
      "LETREC\n"
      ^ String.make indent_next ' '
      ^ "name: "
      ^ Id.to_string (fst fdef.name)
      ^ " "
      ^ Type.print (snd fdef.name)
      ^ "\n"
      ^ String.make indent_next ' '
      ^ "args:"
      ^ List.fold_left
          (fun s arg ->
            s ^ " " ^ Id.to_string (fst arg) ^ ":" ^ Type.print (snd arg))
          "" fdef.args
      ^ "body: \n"
      ^ print_t fdef.body indent_next
      ^ "\n" ^ print_t t indent
  | App (t, tl) ->
      "APP\n" ^ print_t t indent_next
      ^ List.fold_left (fun s t -> s ^ "\n" ^ print_t t indent_next) "" tl
  | Tuple tl ->
      "TUPLE\n"
      ^ List.fold_left (fun s t -> s ^ print_t t indent_next ^ "\n") "" tl
  | LetTuple (idl, t1, t2) ->
      "LETTUPLE\n"
      ^ String.make indent_next ' '
      ^ "vars:"
      ^ List.fold_left
          (fun s (id, t) ->
            s ^ "\n"
            ^ String.make indent_next ' '
            ^ Id.to_string id ^ " " ^ Type.print t)
          "" idl
      ^ "\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | Array (t1, t2) ->
      "Array\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | Get (t1, t2) ->
      "Get\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next
  | Put (t1, t2, t3) ->
      "Put\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next ^ "\n"
      ^ print_t t3 indent_next
  | GlobalTuple tl ->
      "GlobalTuple\n"
      ^ List.fold_left (fun s t -> s ^ print_t t indent_next ^ "\n") "" tl
  | GlobalArray (t1, t2) ->
      "GlobalArray\n" ^ print_t t1 indent_next ^ "\n" ^ print_t t2 indent_next

let print t = print_t t 0
