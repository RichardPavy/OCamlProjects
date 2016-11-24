#!/bin/sh
rm -rf build/
mkdir build
mkdir build/OCamlMake
echo "module OCamlRules = OCamlMake_OCamlRules module Flag = OCamlMake_Flag module OCamlMake = OCamlMake_OCamlMake module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_Make.ml ; cat OCamlMake/Make.ml >> build/OCamlMake/OCamlMake_Make.ml
cp OCamlMake/Make.mli build/OCamlMake/OCamlMake_Make.mli
ocamlfind ocamlc -I build/OCamlMake  -c build/OCamlMake/OCamlMake_Make.mli
echo "module Property = OCamlMake_Property module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_Flag.ml ; cat OCamlMake/Flag.ml >> build/OCamlMake/OCamlMake_Flag.ml
echo "module Property = OCamlMake_Property" > build/OCamlMake/OCamlMake_Flag.mli ; cat OCamlMake/Flag.mli >> build/OCamlMake/OCamlMake_Flag.mli
cp OCamlMake/Property.mli build/OCamlMake/OCamlMake_Property.mli
mkdir build/Container
cp Container/LinkedList.mli build/Container/Container_LinkedList.mli
mkdir build/Utils
cp Utils/Iterable.mli build/Utils/Utils_Iterable.mli
ocamlfind ocamlc -I build/Utils  -c build/Utils/Utils_Iterable.mli
ocamlfind ocamlc -I build/Utils -I build/Container  -c build/Container/Container_LinkedList.mli
cp Utils/File.mli build/Utils/Utils_File.mli
ocamlfind ocamlc -I build/Utils  -c build/Utils/Utils_File.mli
ocamlfind ocamlc -I build/Utils -I build/Container -I build/OCamlMake  -c build/OCamlMake/OCamlMake_Property.mli
echo "module File = Utils_File module Iterable = Utils_Iterable" > build/Utils/Utils_Predicate.mli ; cat Utils/Predicate.mli >> build/Utils/Utils_Predicate.mli
ocamlfind ocamlc -I build/Utils  -c build/Utils/Utils_Predicate.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_Flag.mli
echo "module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_Property.ml ; cat OCamlMake/Property.ml >> build/OCamlMake/OCamlMake_Property.ml
cp Container/LinkedList.ml build/Container/Container_LinkedList.ml
cp Utils/Iterable.ml build/Utils/Utils_Iterable.ml
ocamlfind ocamlopt -I build/Utils -strict-formats -strict-sequence -unsafe -noassert -c build/Utils/Utils_Iterable.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Container/Container_LinkedList.ml
echo "module Utils = Utils_Utils" > build/Utils/Utils_Cache.ml ; cat Utils/Cache.ml >> build/Utils/Utils_Cache.ml
cp Utils/Cache.mli build/Utils/Utils_Cache.mli
ocamlfind ocamlc -I build/Utils  -c build/Utils/Utils_Cache.mli
echo "module LinkedList = Container_LinkedList" > build/Container/Container_LruMap.ml ; cat Container/LruMap.ml >> build/Container/Container_LruMap.ml
cp Container/LruMap.mli build/Container/Container_LruMap.mli
ocamlfind ocamlc -I build/Utils -I build/Container  -c build/Container/Container_LruMap.mli
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Container/Container_LruMap.ml
echo "module Iterable = Utils_Iterable" > build/Utils/Utils_Utils.ml ; cat Utils/Utils.ml >> build/Utils/Utils_Utils.ml
echo "module Iterable = Utils_Iterable" > build/Utils/Utils_Utils.mli ; cat Utils/Utils.mli >> build/Utils/Utils_Utils.mli
ocamlfind ocamlc -I build/Utils  -c build/Utils/Utils_Utils.mli
ocamlfind ocamlopt -I build/Utils -strict-formats -strict-sequence -unsafe -noassert -c build/Utils/Utils_Utils.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Utils/Utils_Cache.ml
echo "module Log = Utils_Log module Foldable = Utils_Foldable" > build/Utils/Utils_File.ml ; cat Utils/File.ml >> build/Utils/Utils_File.ml
cp Container/Slice.ml build/Container/Container_Slice.ml
cp Container/Slice.mli build/Container/Container_Slice.mli
cp Utils/Foldable.mli build/Utils/Utils_Foldable.mli
ocamlfind ocamlc -I build/Utils  -c build/Utils/Utils_Foldable.mli
ocamlfind ocamlc -I build/Utils -I build/Container  -c build/Container/Container_Slice.mli
echo "module Log = Utils_Log" > build/Utils/Utils_Foldable.ml ; cat Utils/Foldable.ml >> build/Utils/Utils_Foldable.ml
cp Utils/Log.ml build/Utils/Utils_Log.ml
cp Utils/Log.mli build/Utils/Utils_Log.mli
ocamlfind ocamlc -I build/Utils  -c build/Utils/Utils_Log.mli
ocamlfind ocamlopt -I build/Utils -strict-formats -strict-sequence -unsafe -noassert -c build/Utils/Utils_Log.ml
ocamlfind ocamlopt -I build/Utils -strict-formats -strict-sequence -unsafe -noassert -c build/Utils/Utils_Foldable.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Container/Container_Slice.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Utils/Utils_File.ml
echo "module Iterable = Utils_Iterable module File = Utils_File" > build/Utils/Utils_Predicate.ml ; cat Utils/Predicate.ml >> build/Utils/Utils_Predicate.ml
ocamlfind ocamlopt -I build/Utils -strict-formats -strict-sequence -unsafe -noassert -package str -c build/Utils/Utils_Predicate.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package str -c build/OCamlMake/OCamlMake_Property.ml
ocamlfind ocamlopt -I build/Utils -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package str -c build/OCamlMake/OCamlMake_Flag.ml
echo "module Timestamp = OCamlMake_Timestamp module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_OCamlMake.ml ; cat OCamlMake/OCamlMake.ml >> build/OCamlMake/OCamlMake_OCamlMake.ml
cp OCamlMake/OCamlMake.mli build/OCamlMake/OCamlMake_OCamlMake.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_OCamlMake.mli
echo "module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_Timestamp.ml ; cat OCamlMake/Timestamp.ml >> build/OCamlMake/OCamlMake_Timestamp.ml
cp OCamlMake/Timestamp.mli build/OCamlMake/OCamlMake_Timestamp.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_Timestamp.mli
cp Container/HashSet.ml build/Container/Container_HashSet.ml
cp Container/HashSet.mli build/Container/Container_HashSet.mli
ocamlfind ocamlc -I build/Utils -I build/Container  -c build/Container/Container_HashSet.mli
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Container/Container_HashSet.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_Timestamp.ml
ocamlfind ocamlopt -I build/Utils -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_OCamlMake.ml
echo "module Flag = OCamlMake_Flag module Timestamp = OCamlMake_Timestamp module CommonRules = OCamlMake_CommonRules module Private = OCamlMake_Private module Process = OCamlMake_Process module OCamlMake = OCamlMake_OCamlMake module Property = OCamlMake_Property module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end module OCamlDep = OCamlMake_OCamlDep module Container = struct   module Slice = Container_Slice   module LinkedList = Container_LinkedList   module LruMap = Container_LruMap   module LinkedHashSet = Container_LinkedHashSet   module LinkedHashMap = Container_LinkedHashMap   module HashSet = Container_HashSet end module Canonical = OCamlMake_Canonical" > build/OCamlMake/OCamlMake_OCamlRules.ml ; cat OCamlMake/OCamlRules.ml >> build/OCamlMake/OCamlMake_OCamlRules.ml
echo "module Flag = OCamlMake_Flag module OCamlMake = OCamlMake_OCamlMake module Property = OCamlMake_Property" > build/OCamlMake/OCamlMake_OCamlRules.mli ; cat OCamlMake/OCamlRules.mli >> build/OCamlMake/OCamlMake_OCamlRules.mli
ocamlfind ocamlc -I build/Utils -I build/Container -I build/OCamlMake  -c build/OCamlMake/OCamlMake_OCamlRules.mli
cp Container/LinkedHashMap.ml build/Container/Container_LinkedHashMap.ml
cp Container/LinkedHashMap.mli build/Container/Container_LinkedHashMap.mli
ocamlfind ocamlc -I build/Utils -I build/Container  -c build/Container/Container_LinkedHashMap.mli
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Container/Container_LinkedHashMap.ml
cp Container/LinkedHashSet.ml build/Container/Container_LinkedHashSet.ml
cp Container/LinkedHashSet.mli build/Container/Container_LinkedHashSet.mli
ocamlfind ocamlc -I build/Utils -I build/Container  -c build/Container/Container_LinkedHashSet.mli
ocamlfind ocamlopt -I build/Utils -I build/Container -strict-formats -strict-sequence -unsafe -noassert -c build/Container/Container_LinkedHashSet.ml
echo "module Private = OCamlMake_Private module Process = OCamlMake_Process module OCamlMake = OCamlMake_OCamlMake module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end module OCamlDep = OCamlMake_OCamlDep module Container = struct   module Slice = Container_Slice   module LinkedList = Container_LinkedList   module LruMap = Container_LruMap   module LinkedHashSet = Container_LinkedHashSet   module LinkedHashMap = Container_LinkedHashMap   module HashSet = Container_HashSet end" > build/OCamlMake/OCamlMake_Canonical.ml ; cat OCamlMake/Canonical.ml >> build/OCamlMake/OCamlMake_Canonical.ml
echo "module OCamlMake = OCamlMake_OCamlMake" > build/OCamlMake/OCamlMake_Canonical.mli ; cat OCamlMake/Canonical.mli >> build/OCamlMake/OCamlMake_Canonical.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_Canonical.mli
echo "module Flag = OCamlMake_Flag module Timestamp = OCamlMake_Timestamp module Private = OCamlMake_Private module Process = OCamlMake_Process module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_OCamlDep.ml ; cat OCamlMake/OCamlDep.ml >> build/OCamlMake/OCamlMake_OCamlDep.ml
cp OCamlMake/OCamlDep.mli build/OCamlMake/OCamlMake_OCamlDep.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_OCamlDep.mli
echo "module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_Private.ml ; cat OCamlMake/Private.ml >> build/OCamlMake/OCamlMake_Private.ml
cp OCamlMake/Private.mli build/OCamlMake/OCamlMake_Private.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_Private.mli
ocamlfind ocamlopt -I build/Utils -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package str -c build/OCamlMake/OCamlMake_Private.ml
echo "module Timestamp = OCamlMake_Timestamp module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_Process.ml ; cat OCamlMake/Process.ml >> build/OCamlMake/OCamlMake_Process.ml
cp OCamlMake/Process.mli build/OCamlMake/OCamlMake_Process.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_Process.mli
ocamlfind ocamlopt -I build/Utils -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_Process.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_OCamlDep.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_Canonical.ml
echo "module Timestamp = OCamlMake_Timestamp module Private = OCamlMake_Private module Process = OCamlMake_Process module OCamlMake = OCamlMake_OCamlMake module Utils = struct   module Iterable = Utils_Iterable   module Predicate = Utils_Predicate   module Utils = Utils_Utils   module Log = Utils_Log   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache end" > build/OCamlMake/OCamlMake_CommonRules.ml ; cat OCamlMake/CommonRules.ml >> build/OCamlMake/OCamlMake_CommonRules.ml
echo "module OCamlMake = OCamlMake_OCamlMake" > build/OCamlMake/OCamlMake_CommonRules.mli ; cat OCamlMake/CommonRules.mli >> build/OCamlMake/OCamlMake_CommonRules.mli
ocamlfind ocamlc -I build/Utils -I build/OCamlMake  -c build/OCamlMake/OCamlMake_CommonRules.mli
ocamlfind ocamlopt -I build/Utils -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_CommonRules.ml
ocamlfind ocamlopt -I build/Utils -I build/Container -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_OCamlRules.ml
ocamlfind ocamlopt -I build/Utils -I build/OCamlMake -strict-formats -strict-sequence -unsafe -noassert -package unix -package str -c build/OCamlMake/OCamlMake_Make.ml
ocamlfind ocamlopt -I build/Utils -I build/OCamlMake -noassert -linkpkg -package unix -package str -o build/OCamlMake/OCamlMake_Make.exe build/Utils/Utils_Iterable.cmx build/Container/Container_LinkedList.cmx build/Container/Container_LruMap.cmx build/Utils/Utils_Utils.cmx build/Utils/Utils_Cache.cmx build/Utils/Utils_Log.cmx build/Utils/Utils_Foldable.cmx build/Container/Container_Slice.cmx build/Utils/Utils_File.cmx build/Utils/Utils_Predicate.cmx build/OCamlMake/OCamlMake_Property.cmx build/OCamlMake/OCamlMake_Flag.cmx build/Container/Container_HashSet.cmx build/OCamlMake/OCamlMake_Timestamp.cmx build/OCamlMake/OCamlMake_OCamlMake.cmx build/Container/Container_LinkedHashMap.cmx build/Container/Container_LinkedHashSet.cmx build/OCamlMake/OCamlMake_Private.cmx build/OCamlMake/OCamlMake_Process.cmx build/OCamlMake/OCamlMake_OCamlDep.cmx build/OCamlMake/OCamlMake_Canonical.cmx build/OCamlMake/OCamlMake_CommonRules.cmx build/OCamlMake/OCamlMake_OCamlRules.cmx build/OCamlMake/OCamlMake_Make.cmx
cp build/OCamlMake/OCamlMake_Make.exe OCamlMake/Make.exe
rm -rf build
find . -type f -name '*.cmi' -exec rm {} +
find . -type f -name '*.cmo' -exec rm {} +
find . -type f -name '*.o' -exec rm {} +
find . -type f -name '*.cmx' -exec rm {} +
find . -type f -name '*.a' -exec rm {} +
