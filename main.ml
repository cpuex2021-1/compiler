let verbose = ref false

let compile in_channel out_channel =
  let lexbuf = Lexing.from_channel in_channel in
  let parsed = Parser.exp Lexer.token lexbuf in
  if !verbose then (
    print_endline "[Parsed]";
    print_endline (Syntax.print parsed));

  (* Syntax.t -> Syntax.t *)
  let typed = TypeCheck.f parsed in
  if !verbose then (
    print_endline "[Typed]";
    print_endline (Syntax.print typed));

  (* Syntax.t -> Normalize.t *)
  let normalized = Normalize.kNorm typed in
  if !verbose then (
    print_endline "[Normalized]";
    print_endline (Normalize.print normalized));

  (* Normalize.t -> Normalize.t *)
  let transformed = Normalize.alpha normalized in
  if !verbose then (
    print_endline "[Alpha Transformed]";
    print_endline (Normalize.print transformed));

  (* Normalize.t -> Closure.prog *)
  let closured = Closure.f transformed in
  if !verbose then (
    print_endline "[Closured]";
    print_endline (Closure.print closured));

  (* Closure.prog -> Asm.prog *)
  let virtual_asm = Virtual.f closured in
  if !verbose then (
    print_endline "[Virtual Asm]";
    print_endline (Virtual.print virtual_asm));

  (* Asm.prog -> Asm.prog *)
  let allocated = RegAlloc.f virtual_asm in
  if !verbose then (
    print_endline "[Register Allocated]";
    print_endline (Virtual.print allocated));

  (* Asm.prog -> output *)
  Emit.f out_channel allocated

let file f =
  let in_channel = open_in f in
  let out_channel = open_out "out.s" in
  compile in_channel out_channel

let () =
  let f = ref "" in
  Arg.parse
    [
      ( "-v",
        Arg.Bool (fun i -> verbose := i),
        "whether to output interim program" );
    ]
    (fun s -> f := s)
    "";
  ignore (file !f)
