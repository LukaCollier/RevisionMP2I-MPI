onitama: utilities.cmo types.cmo io.cmo gameengine.cmo ai.cmo main.ml
	ocamlfind ocamlc -o onitama -package graphics graphics.cma $^

types.cmo: types.ml
	ocamlc -c $^

utilities.cmo: utilities.ml
	ocamlc -c $^

io.cmo: types.cmo io.ml
	ocamlfind ocamlc -c -package graphics graphics.cma $^

gameengine.cmo: types.cmo io.cmo gameengine.ml
	ocamlc -c $^

ai.cmo: gameengine.cmo ai.ml
	ocamlc -c $^

clean:
	rm -rf *.cmi *.cmo onitama
