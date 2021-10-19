{
    open Parser
    open Type
}

let space = [' ' '\t' '\r']
let newline = ['\n']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| newline
    { Lexing.new_line lexbuf; token lexbuf }
| "(*"
    { comment lexbuf; token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
(* | "true"
    { BOOL(true) }
| "false"
    { BOOL(false) } *)
| "not"
    { NOT }
| digit+ 
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-' 
    { MINUS }
| '+' 
    { PLUS }
| "-."
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| "*."
    { AST_DOT }
| "/."
    { SLASH_DOT }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| ','
    { COMMA }
| '_'
    { IDENT(Id.gentmp Type.Unit) }
| "Array.create" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| eof
    { EOF }
| lower (digit|lower|upper|'_')* 
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
    (let pos = lexbuf.lex_curr_p in 
    Printf.sprintf "**Lex error at line %d char %d.**"
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| newline
    { Lexing.new_line lexbuf; comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }