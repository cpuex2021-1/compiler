SOURCES = type.ml syntax.ml id.ml lexer.mll parser.mly typeCheck.ml eval.ml main.ml
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile