let rec env_exists x env =
  match env with
  | (a, b) :: xs -> if a = x then true else env_exists x xs
  | [] -> false

let rec env_find x env =
  match env with
  | (a, b) :: xs -> if a = x then b else env_find x xs
  | [] -> raise Not_found

let env_find2 x env = try env_find x env with Not_found -> x

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
