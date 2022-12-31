all: $(patsubst %.ml,%.byte,$(wildcard *.ml))

%.byte: %.ml
	ocamlbuild -use-ocamlfind $@

%: %.byte
	./$<

clean:
	git clean -dfX

.PHONY: all clean