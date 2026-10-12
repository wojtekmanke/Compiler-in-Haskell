# Builds the compiler (latc) and the runtime library that the latc_x86_64
# driver links into every compiled program.

CABAL ?= cabal

sources := latte.cabal $(shell find app src -type f)

.PHONY: all test grammar clean

all: latc runtime/runtime.o

latc: $(sources)
	$(CABAL) build exe:latc
	cp "$$($(CABAL) list-bin exe:latc)" $@

runtime/runtime.o: runtime/runtime.c
	$(CC) -O2 -Wall -Wextra -c $< -o $@

test: all
	tests/run.sh

# Regenerates the lexer, the parser and the syntax tree from the grammar.
# Requires BNFC (https://bnfc.digitalgrammars.com), version 2.9 or later.
grammar:
	tmp=$$(mktemp -d) \
	  && bnfc --haskell --functor -d -o "$$tmp" grammar/Latte.cf \
	  && cp "$$tmp"/Latte/Abs.hs "$$tmp"/Latte/Lex.x "$$tmp"/Latte/Par.y src/Latte/ \
	  && rm -r "$$tmp"

clean:
	$(CABAL) clean
	rm -f latc runtime/runtime.o
