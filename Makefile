SOURCES = \
	id.ml \
	type.ml \
	syntax.ml \
	ds.ml \
	lexer.mll \
	parser.mly \
	typeCheck.ml \
	normalize.ml \
	opt.ml \
	closure.ml \
	asm.ml \
	virtual.ml \
	regAlloc.ml \
	emit.ml \
	eval.ml \
	main.ml
RESULT  = main

YFLAGS = -v 

all: byte-code byte-code-library

-include OCamlMakefile