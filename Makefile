.PHONY: all pl clean
LOAD ?= test2.txt
TARGET = main

all: 
	@ocamlc -c helper.ml 
	@ocamlc -c interpreter.ml 
	@ocamlyacc parser.mly 
	@ocamlc -c parser.mli 
	@ocamlc -c parser.ml 
	@ocamllex lexer.mll > /dev/null 2>&1
	@ocamlc -c lexer.ml 
	@ocamlc -c main.ml 
	@ocamlc -o $(TARGET) helper.cmo interpreter.cmo parser.cmo lexer.cmo main.cmo 
	@./main programs/$(LOAD)

clean:
	@rm -f *.cmo *.cmi parser.ml parser.mli lexer.ml
	@rm -f $(TARGET)
	@echo "All object files removed."
