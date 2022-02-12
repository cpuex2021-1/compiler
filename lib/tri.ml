let rec pi_div e x =
  if 0. = e then e
  else if 0. < e then(
    if e < 3.141592653 *. 2. then e
    else if x >= e then pi_div (e -. fhalf x) (fhalf x)
    else pi_div e (x *. 2.))
  else if x >= (-.e) then pi_div (e +. x) (fhalf x)
  else pi_div e (x *. 2.)
in

let rec pi4div x =
  if x < 3.141592653 /. 2. then (x, 1.)
  else if x < 3.141592653 then (3.141592653 -. x, -1.)
  else if x < 3.141592653 *. 1.5 then (x -. 3.141592653, -1.)
  else ((3.141592653 *. 2.) -. x, 1.)
in

let rec pi4div2 x =
  if x < 3.141592653 /. 2. then (x, 1.)
  else if x < 3.141592653 then (3.141592653 -. x, 1.)
  else if x < 3.141592653 *. 1.5 then (x -. 3.141592653, -1.)
  else ((3.141592653 *. 2.) -. x, -1.)
in

let rec tailor_cos1 y =
  let xx = y *. y in
  let t2 = fhalf xx in
  let t4 = xx *. t2 *. 0.08333333333 in
  let t6 = xx *. t4 *. 0.03333333333 in
  let t8 = xx *. t6 *. 0.01785714285 in
  let t10 = xx *. t8 *. 0.01111111111 in
  let t12 = xx *. t10 *. 0.00757575757 in
  1. -. t2 +. t4 -. t6 +. t8 -. t10 +. t12
in

let rec cos1 x =
  let (a, b) = pi4div (pi_div x (3.141592653 *. 2.)) in
  b *. tailor_cos1 a
in

let rec sin1 x =
  let (a, b) = pi4div2 (pi_div x (3.141592653 *. 2.)) in
  b *. tailor_cos1 ((3.141592653 /. 2.) -. a)
in

let rec tailor_atan1 y =
  let xx = y *. y in
  let t3 = xx *. y *. 0.33333333333 in
  let t5 = xx *. t3 *. 0.6 in
  let t7 = xx *. t5 *. 0.71428571428 in
  let t9 = xx *. t7 *. 0.77777777777 in
  let t11 = xx *. t9 *. 0.81818181818 in
  y -. t3 +. t5 -. t7 +. t9 -. t11
in

let rec atan1 y =
  if y < 0. then -.atan1 (-.y)
  else if y > 1. then (3.141592653 /. 2.) -. atan1 (1. /. y)
  else if y > 0.41421356 then (3.141592653 /. 4.) +. atan1 ((y -. 1.) /. (1. +. y))
  else tailor_atan1 y
in

print_int (sin1 (0. -. 1.01))
