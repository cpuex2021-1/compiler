let compile in_channel out_channel =
  let lexbuf = Lexing.from_channel in_channel in
  let parsed = Parser.exp Lexer.token lexbuf in
  let typed = TypeCheck.f parsed in
  print_endline (Syntax.print parsed);
  print_endline (Syntax.print typed)

let file f =
  let in_channel = open_in f in
  let out_channel = open_out "out.s" in
  compile in_channel out_channel

let () =
  let f = ref "" in
  Arg.parse [] (fun s -> f := s) "";
  ignore (file !f)
