default:
	ocamlbuild example.native

.PHONY: clean

clean:
	ocamlbuild -clean

