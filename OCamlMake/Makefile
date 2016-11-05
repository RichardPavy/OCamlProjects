FLAGS = -package unix,str

OM = Bootstrap
DEPS = \
    ../Utils/Log Bootstrap/Utils_Log \
    ../Utils/Iterable Bootstrap/Utils_Iterable \
    ../Utils/Foldable \
    ../Utils/Utils \
    ../Container/HashSet \
    ../Container/LinkedHashSet \
    ../Container/LinkedHashMap \
    ../Container/LinkedList \
    ../Container/LruMap \
    ../Utils/Cache \
    ../Utils/File \
    ../Utils/Predicate \
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

INCLUDE_DIRS = \
    -I ../Container \
    -I ../Utils \
    -I Bootstrap

all: Make.exe

$(OM:=.exe): $(foreach dep, $(DEPS), -intf $(dep).mli $(dep).ml)
	ocamlfind opt $(INCLUDE_DIRS) -o $@ -linkpkg $(FLAGS) $^

Make.exe: $(OM:=.exe)
	make clean
	./$(OM:=.exe) -target $@
	make clean

clean:
	rm -rf build/
	(cd .. ; make clean)

.PHONY: test clean
