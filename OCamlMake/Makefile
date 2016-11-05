FLAGS = -package unix,str

OM = OCamlMake
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
    Private \
    OCamlDep \
    OCamlMake \
    CommonRules \
    Canonical \
    CommonRules \
    OCamlRules \
    Make

all: Make.exe

$(OM:=.exe): $(DEPS:=.mli) $(DEPS:=.ml) #$(OM:=.mli) $(OM:=.ml)
	ocamlfind opt -I ../Container -o $@ -linkpkg $(FLAGS) $^

Make.exe: $(OM:=.exe)
	make clean
	./$(OM:=.exe) -target $@
	make clean

clean:
	rm -rf build/
	(cd .. ; make clean)

.PHONY: test clean
