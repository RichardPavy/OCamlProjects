FLAGS = -package unix,str

OB = OCamlBuild
DEPS = \
    Log \
    Iterable \
    Foldable \
    Utils \
    HashSet \
    LinkedHashSet \
    LinkedHashMap \
    LinkedList \
    LruMap \
    Cache \
    File \
    Predicate \
    Timestamp \
    Process \
    Property \
    Flag \
    OCamlDep \
    OCamlBuild \
    CommonRules \
    OCamlRules \
    Main

all: $(OB:=.cma) $(OB:=.cmxa)

$(OB:=.cma): $(DEPS:=.mli) $(DEPS:=.ml) #$(OB:=.mli) $(OB:=.ml)
	ocamlfind ocamlc -o $@ -a $(FLAGS) $^

$(OB:=.cmxa): $(DEPS:=.mli) $(DEPS:=.ml) #$(OB:=.mli) $(OB:=.ml)
	ocamlfind opt -o $@ -a $(FLAGS) $^

$(OB:=.test): $(DEPS:=.mli) $(DEPS:=.ml) #$(OB:=.mli) $(OB:=.ml)
	ocamlfind opt -o $@ -linkpkg $(FLAGS) $^

$(OB:=.exe): $(DEPS:=.mli) $(DEPS:=.ml) #$(OB:=.mli) $(OB:=.ml)
	ocamlfind opt -o $@ -linkpkg -noassert $(FLAGS) $^

test: OCamlBuild.test
	./OCamlBuild.test

Main.exe: OCamlBuild.exe
	make clean
	./OCamlBuild.exe -target Main.exe

clean:
	(cd .. ; make clean)

.PHONY: test clean
