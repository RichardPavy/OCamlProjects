BUILDER = Builder.exe

OCamlBuild/OCamlBuild.cmxa: OCamlBuild/OCamlBuild.mli OCamlBuild/OCamlBuild.ml
	cd OCamlBuild && make OCamlBuild.cmxa

$(BUILDER): Builder.ml OCamlBuild/OCamlBuild.cmxa
	ocamlfind ocamlopt -o $@ -noassert -linkpkg -package unix,str -I OCamlBuild OCamlBuild.cmxa $<

server: $(BUILDER)
	./$(BUILDER) -target Main.exe
	./Main.exe
	-killall Main.exe

test: $(BUILDER)
	./$(BUILDER) -debug -target Test.d.exe
	time ./Test.d.exe
	killall Test.d.exe

clean:
	find . -type f -name '*.cmi' -exec rm {} +
	find . -type f -name '*.cmo' -exec rm {} +
	find . -type f -name '*.o' -exec rm {} +
	find . -type f -name '*.cmx' -exec rm {} +
	find . -type f -name '*.a' -exec rm {} +
	find . -type f -name '*~' -exec rm {} +
	find . -type f -name '*.dependencies' -exec rm {} +
	find . -type f -name '*.modules' -exec rm {} +
	find Static/Files -type f -name '*.ml' -exec rm {} +

.PHONY: server test clean
