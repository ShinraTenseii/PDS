SRC=$(wildcard *.ml *.mll)

.PHONY: clean

main.native: $(SRC)
	ocamlbuild -verbose 0 -package str -pp camlp4o $@

clean:
	ocamlbuild -clean
