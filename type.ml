type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var (ref None)

let rec print t =
  match t with
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | Fun (tl, t) ->
      "Fun ("
      ^ List.fold_left (fun s t -> s ^ print t) "" tl
      ^ ") -> " ^ print t
  | Tuple tl -> "(" ^ List.fold_left (fun s t -> s ^ print t) "" tl ^ ")"
  | Array t -> "Array" ^ print t
  | Var t -> "Var"
