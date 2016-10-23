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
    CommonRules \
    OCamlDep \
    OCamlMake \
    Canonical \
    CommonRules \
    OCamlRules \
    Make

all: Make.exe

$(OM:=.exe): $(DEPS:=.mli) $(DEPS:=.ml) #$(OM:=.mli) $(OM:=.ml)
	ocamlfind opt -o $@ -linkpkg $(FLAGS) $^

Make.exe: $(OM:=.exe)
	rm -rf build/
	./$(OM:=.exe) -target $@

clean:
	(cd .. ; make clean)

.PHONY: test clean
