let verbose = ref false

let iter = ref 0

let compile in_channel out_channel =
  let lexbuf = Lexing.from_channel in_channel in
  let parsed = Parser.exp Lexer.token lexbuf in
  if !verbose then (
    let oc = open_out "output/01-parsed.txt" in
    Printf.fprintf oc "[Parsed]\n";
    Printf.fprintf oc "%s\n" (Syntax.print parsed));

  (* Syntax.t -> Syntax.t *)
  let typed = TypeCheck.f parsed in
  if !verbose then (
    let oc = open_out "output/02-typed.txt" in
    Printf.fprintf oc "[Typed]";
    Printf.fprintf oc "%s\n" (Syntax.print typed));

  (* Syntax.t -> Normalize.t *)
  let normalized = Normalize.kNorm typed in
  if !verbose then (
    let oc = open_out "output/03-normalized.txt" in
    Printf.fprintf oc "[Normalized]";
    Printf.fprintf oc "%s\n" (Normalize.print normalized));

  (* Normalize.t -> Normalize.t *)
  let transformed = Normalize.alpha normalized in
  if !verbose then (
    let oc = open_out "output/04-alpha-transformed.txt" in
    Printf.fprintf oc "[Alpha Transformed]";
    Printf.fprintf oc "%s\n" (Normalize.print transformed));

  (* Normalize.t -> Normalize.t *)
  let optimized = Opt.f transformed !iter in
  if !verbose then (
    let oc = open_out "output/04.5-optimized.txt" in
    Printf.fprintf oc "[Optimized]";
    Printf.fprintf oc "%s\n" (Normalize.print optimized));

  (* Normalize.t -> Closure.prog *)
  let closured = Closure.f optimized in
  if !verbose then (
    let oc = open_out "output/05-closured.txt" in
    Printf.fprintf oc "[Closured]";
    Printf.fprintf oc "%s\n" (Closure.print closured));

  (* Closure.prog -> Asm.prog *)
  let virtual_asm = Virtual.f closured in
  if !verbose then (
    let oc = open_out "output/06-virtual-asm.txt" in
    Printf.fprintf oc "[Virtual Asm]";
    Printf.fprintf oc "%s\n" (Virtual.print virtual_asm));

  (* Asm.prog -> Asm.prog *)
  let allocated = RegAlloc.f virtual_asm in
  if !verbose then (
    let oc = open_out "output/07-register-allocated.txt" in
    Printf.fprintf oc "[Register Allocated]";
    Printf.fprintf oc "%s\n" (Virtual.print allocated));

  (* Asm.prog -> output *)
  Emit.f out_channel allocated

let file f =
  let in_channel = open_in f in
  let out_channel = open_out "output/99-asm.s" in
  compile in_channel out_channel

let () =
  let f = ref "" in
  Arg.parse
    [
      ( "-v",
        Arg.Bool (fun i -> verbose := i),
        "whether to output interim program" );
      ( "-i",
        Arg.Int (fun i -> iter := i),
        "the number of iterations for optimization" );
    ]
    (fun s -> f := s)
    "";
  ignore (file !f)
