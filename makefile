all: evaluation expr evaluation_tests expr_tests miniml

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

evaluation_tests: evaluation_tests.ml
	ocamlbuild -use-ocamlfind evaluation_tests.byte

expr_tests: expr_tests.ml
	ocamlbuild -use-ocamlfind expr_tests.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

clean:
	rm -rf _build *.byte