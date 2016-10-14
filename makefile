# DLP - Practice 1
# OCaml Makefile
#
# Xoan Andreu Barro Torres (andreu.barro)
# Manoel Anton Folgueira Hernandez (manoel.folgueira)
# Uxia Ponte Villaverde (uxia.ponte.villaverde)

OFLAGS = -g

# Compilation
OC = ocamlc
LIB = str.cma
OUTPUTFILE = -o main

# Execution
dfa = example.dfa

# Documentation
OD = ocamldoc
ODFMT = -html


default: compile

compile:
	rm -Rf target && mkdir target
	$(OC) $(OFLAGS) $(OUTPUTFILE) $(LIB) \
		genlist.ml state.ml symbol.ml transition.ml alphabet.ml \
		dfa.mli dfa.ml \
		main.mli main.ml
	mv -t ./target main *.cmo *.cmi

run:
	./target/main $(dfa)

doc: compile
	rm -Rf doc && mkdir doc
	$(OD) -d doc -I target $(ODFMT) *.mli *.ml

clean:
	$(RM) -Rf target doc
