#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)


######################### BUILDING INFOS ######################
# Nom du / des execs
NAME = computorv2
NAME_TESTS = tests

export OUNIT_CI=true


######################### BUILDING STAGES ######################

.PHONY: all clean byte native sanity test

OCB_FLAGS = -use-ocamlfind -I srcs -I tests
OCB_MENHIR_FLAGS = -use-menhir
OCB = ocamlbuild $(OCB_MENHIR_FLAGS) $(OCB_FLAGS)

all: native #byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) $(NAME).native

byte: sanity
	$(OCB) $(NAME).byte

test: sanity
	$(OCB) $(NAME_TESTS).byte
	./$(NAME_TESTS).byte -ci true

# check that packages can be found
sanity:
	ocamlfind query ounit2
	ocamlfind query menhir

