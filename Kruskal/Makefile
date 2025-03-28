kruskal: unionfind.cmo kruskal.ml
	ocamlfind ocamlc -o kruskal -package graphics graphics.cma unionfind.cmo kruskal.ml

unionfind.cmo:
	ocamlc -c unionfind.ml

clean:
	rm -rf unionfind.cmo unionfind.cmi kruskal.cmi kruskal.cmo kruskal
