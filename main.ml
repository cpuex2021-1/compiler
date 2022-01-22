let verbose = ref false

let iter = ref 0

let inline_th = ref 5

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
    Printf.fprintf oc "[Typed]\n";
    Printf.fprintf oc "%s\n" (Syntax.print typed));

  (* Syntax.t -> Normalize.t *)
  let normalized = Normalize.kNorm typed in
  if !verbose then (
    let oc = open_out "output/03-normalized.txt" in
    Printf.fprintf oc "[Normalized]\n";
    Printf.fprintf oc "%s\n" (Normalize.print normalized));

  (* Normalize.t -> Normalize.t *)
  let transformed = Normalize.alpha normalized in
  if !verbose then (
    let oc = open_out "output/04-alpha-transformed.txt" in
    Printf.fprintf oc "[Alpha Transformed]\n";
    Printf.fprintf oc "%s\n" (Normalize.print transformed));

  (* Normalize.t -> Normalize.t *)
  let optimized = Opt.f transformed !iter !inline_th in
  if !verbose then (
    let oc = open_out "output/04.5-optimized.txt" in
    Printf.fprintf oc "[Optimized]\n";
    Printf.fprintf oc "%s\n" (Normalize.print optimized));

  (* Normalize.t -> Closure.prog *)
  let closured = Closure.f optimized in
  if !verbose then (
    let oc = open_out "output/05-closured.txt" in
    Printf.fprintf oc "[Closured]\n";
    Printf.fprintf oc "%s\n" (Closure.print closured));

  (* Closure.prog -> Closure.prog *)
  let csed = Closure.cse closured in
  if !verbose then (
    let oc = open_out "output/05.5-csed.txt" in
    Printf.fprintf oc "[CSEd]\n";
    Printf.fprintf oc "%s\n" (Closure.print csed));

  (* Closure.prog -> Asm.prog *)
  let virtual_asm = Virtual.f csed in
  if !verbose then (
    let oc = open_out "output/06-virtual-asm.txt" in
    Printf.fprintf oc "[Virtual Asm]\n";
    Printf.fprintf oc "%s\n" (Virtual.print virtual_asm));

  (* Asm.prog -> Asm.prog *)
  let scheduled = Sched.f virtual_asm in
  if !verbose then (
    let oc = open_out "output/06.1-scheduled.txt" in
    Printf.fprintf oc "[Scheduled]\n";
    Printf.fprintf oc "%s\n" (Virtual.print scheduled));

  (* Asm.prog -> Asm.prog *)
  let simm_asm = Virtual.simm scheduled in
  if !verbose then (
    let oc = open_out "output/06.5-simm-asm.txt" in
    Printf.fprintf oc "[Simm Asm]\n";
    Printf.fprintf oc "%s\n" (Virtual.print simm_asm));

  (* Asm.prog -> Asm.prog *)
  let allocated = RegAlloc.f simm_asm in
  if !verbose then (
    let oc = open_out "output/07-register-allocated.txt" in
    Printf.fprintf oc "[Register Allocated]\n";
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
      ( "-t",
        Arg.Int (fun i -> inline_th := i),
        "the maximum size of function to be iinlined" );
    ]
    (fun s -> f := s)
    "";
  ignore (file !f)
