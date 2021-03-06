mkdir build
mkdir build/OCamlMake
echo "module CommonRules = OCamlMake_CommonRules module OCamlMake = OCamlMake_OCamlMake module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end module BootstrapRule = OCamlMake_BootstrapRule" > build/OCamlMake/OCamlMake_Make.ml ; cat OCamlMake/Make.ml >> build/OCamlMake/OCamlMake_Make.ml
cp OCamlMake/Make.mli build/OCamlMake/OCamlMake_Make.mli
ocamlfind ocamlc -I build/OCamlMake -c build/OCamlMake/OCamlMake_Make.mli
echo "module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module CommonRules = OCamlMake_CommonRules module OCamlMake = OCamlMake_OCamlMake module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCamlMake_BootstrapRule.ml ; cat OCamlMake/BootstrapRule.ml >> build/OCamlMake/OCamlMake_BootstrapRule.ml
echo "module OCamlMake = OCamlMake_OCamlMake module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCamlMake_BootstrapRule.mli ; cat OCamlMake/BootstrapRule.mli >> build/OCamlMake/OCamlMake_BootstrapRule.mli
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCamlMake_OCamlMake.mli ; cat OCamlMake/OCamlMake.mli >> build/OCamlMake/OCamlMake_OCamlMake.mli
mkdir build/Utils
echo "module File = Utils_File" > build/Utils/Utils_Asserts.mli ; cat Utils/Asserts.mli >> build/Utils/Utils_Asserts.mli
echo "module Foldable = Utils_Foldable module Iterable = Utils_Iterable" > build/Utils/Utils_File.mli ; cat Utils/File.mli >> build/Utils/Utils_File.mli
cp Utils/Foldable.mli build/Utils/Utils_Foldable.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_Foldable.mli
cp Utils/Iterable.mli build/Utils/Utils_Iterable.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_Iterable.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_File.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_Asserts.mli
cp Utils/Cache.mli build/Utils/Utils_Cache.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_Cache.mli
cp Utils/CommandLine.mli build/Utils/Utils_CommandLine.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_CommandLine.mli
cp Utils/Log.mli build/Utils/Utils_Log.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_Log.mli
echo "module File = Utils_File module Iterable = Utils_Iterable" > build/Utils/Utils_Predicate.mli ; cat Utils/Predicate.mli >> build/Utils/Utils_Predicate.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_Predicate.mli
echo "module Iterable = Utils_Iterable" > build/Utils/Utils_Utils.mli ; cat Utils/Utils.mli >> build/Utils/Utils_Utils.mli
ocamlfind ocamlc -I build/Utils -c build/Utils/Utils_Utils.mli
ocamlfind ocamlc -I build/OCamlMake -I build/Utils -c build/OCamlMake/OCamlMake_OCamlMake.mli
ocamlfind ocamlc -I build/OCamlMake -I build/Utils -c build/OCamlMake/OCamlMake_BootstrapRule.mli
echo "module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module OCamlMake = OCamlMake_OCamlMake module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCamlMake_CommonRules.ml ; cat OCamlMake/CommonRules.ml >> build/OCamlMake/OCamlMake_CommonRules.ml
echo "module OCamlMake = OCamlMake_OCamlMake module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCamlMake_CommonRules.mli ; cat OCamlMake/CommonRules.mli >> build/OCamlMake/OCamlMake_CommonRules.mli
ocamlfind ocamlc -I build/OCamlMake -I build/Utils -c build/OCamlMake/OCamlMake_CommonRules.mli
mkdir build/OCamlMake/Common
echo "module Property = OCamlMake_Common_Property module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Flag.ml ; cat OCamlMake/Common/Flag.ml >> build/OCamlMake/Common/OCamlMake_Common_Flag.ml
echo "module Property = OCamlMake_Common_Property module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Flag.mli ; cat OCamlMake/Common/Flag.mli >> build/OCamlMake/Common/OCamlMake_Common_Flag.mli
echo "module Container : sig   module Slice = Container_Slice   module LinkedList = Container_LinkedList   module LruMap = Container_LruMap   module LinkedHashSet = Container_LinkedHashSet   module LinkedHashMap = Container_LinkedHashMap   module HashSet = Container_HashSet end module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Property.mli ; cat OCamlMake/Common/Property.mli >> build/OCamlMake/Common/OCamlMake_Common_Property.mli
mkdir build/Container
cp Container/HashSet.mli build/Container/Container_HashSet.mli
ocamlfind ocamlc -I build/Container -I build/Utils -c build/Container/Container_HashSet.mli
cp Container/LinkedHashMap.mli build/Container/Container_LinkedHashMap.mli
ocamlfind ocamlc -I build/Container -I build/Utils -c build/Container/Container_LinkedHashMap.mli
cp Container/LinkedHashSet.mli build/Container/Container_LinkedHashSet.mli
ocamlfind ocamlc -I build/Container -I build/Utils -c build/Container/Container_LinkedHashSet.mli
cp Container/LinkedList.mli build/Container/Container_LinkedList.mli
ocamlfind ocamlc -I build/Container -I build/Utils -c build/Container/Container_LinkedList.mli
cp Container/LruMap.mli build/Container/Container_LruMap.mli
ocamlfind ocamlc -I build/Container -I build/Utils -c build/Container/Container_LruMap.mli
cp Container/Slice.mli build/Container/Container_Slice.mli
ocamlfind ocamlc -I build/Container -I build/Utils -c build/Container/Container_Slice.mli
ocamlfind ocamlc -I build/OCamlMake/Common -I build/Container -I build/Utils -c build/OCamlMake/Common/OCamlMake_Common_Property.mli
ocamlfind ocamlc -I build/OCamlMake/Common -I build/Utils -c build/OCamlMake/Common/OCamlMake_Common_Flag.mli
echo "module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end module Container = struct   module Slice = Container_Slice   module LinkedList = Container_LinkedList   module LruMap = Container_LruMap   module LinkedHashSet = Container_LinkedHashSet   module LinkedHashMap = Container_LinkedHashMap   module HashSet = Container_HashSet end" > build/OCamlMake/Common/OCamlMake_Common_Property.ml ; cat OCamlMake/Common/Property.ml >> build/OCamlMake/Common/OCamlMake_Common_Property.ml
cp Container/HashSet.ml build/Container/Container_HashSet.ml
cp Utils/Iterable.ml build/Utils/Utils_Iterable.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -c build/Utils/Utils_Iterable.ml
echo "module Iterable = Utils_Iterable" > build/Utils/Utils_Utils.ml ; cat Utils/Utils.ml >> build/Utils/Utils_Utils.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -c build/Utils/Utils_Utils.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Container -I build/Utils -c build/Container/Container_HashSet.ml
cp Container/LinkedHashMap.ml build/Container/Container_LinkedHashMap.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Container -I build/Utils -c build/Container/Container_LinkedHashMap.ml
cp Container/LinkedHashSet.ml build/Container/Container_LinkedHashSet.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Container -I build/Utils -c build/Container/Container_LinkedHashSet.ml
cp Container/LinkedList.ml build/Container/Container_LinkedList.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Container -I build/Utils -c build/Container/Container_LinkedList.ml
echo "module LinkedList = Container_LinkedList" > build/Container/Container_LruMap.ml ; cat Container/LruMap.ml >> build/Container/Container_LruMap.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Container -I build/Utils -c build/Container/Container_LruMap.ml
cp Container/Slice.ml build/Container/Container_Slice.ml
echo "module Log = Utils_Log" > build/Utils/Utils_Foldable.ml ; cat Utils/Foldable.ml >> build/Utils/Utils_Foldable.ml
cp Utils/Log.ml build/Utils/Utils_Log.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -c build/Utils/Utils_Log.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -c build/Utils/Utils_Foldable.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Container -I build/Utils -c build/Container/Container_Slice.ml
echo "module Log = Utils_Log module Utils = Utils_Utils module File = Utils_File" > build/Utils/Utils_Asserts.ml ; cat Utils/Asserts.ml >> build/Utils/Utils_Asserts.ml
echo "module Iterable = Utils_Iterable module Log = Utils_Log module Foldable = Utils_Foldable" > build/Utils/Utils_File.ml ; cat Utils/File.ml >> build/Utils/Utils_File.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -I build/Container -c build/Utils/Utils_File.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -c build/Utils/Utils_Asserts.ml
echo "module Asserts = Utils_Asserts module Log = Utils_Log module Utils = Utils_Utils" > build/Utils/Utils_Cache.ml ; cat Utils/Cache.ml >> build/Utils/Utils_Cache.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -I build/Container -c build/Utils/Utils_Cache.ml
echo "module Iterable = Utils_Iterable module Asserts = Utils_Asserts module Log = Utils_Log" > build/Utils/Utils_CommandLine.ml ; cat Utils/CommandLine.ml >> build/Utils/Utils_CommandLine.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -I build/Container -c build/Utils/Utils_CommandLine.ml
echo "module Iterable = Utils_Iterable module File = Utils_File" > build/Utils/Utils_Predicate.ml ; cat Utils/Predicate.ml >> build/Utils/Utils_Predicate.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/Utils -package str -c build/Utils/Utils_Predicate.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/Common -I build/Container -I build/Utils -package str -c build/OCamlMake/Common/OCamlMake_Common_Property.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/Common -I build/Utils -package str -c build/OCamlMake/Common/OCamlMake_Common_Flag.ml
echo "module Timestamp = OCamlMake_Common_Timestamp module Process = OCamlMake_Common_Process module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_FolderContent.ml ; cat OCamlMake/Common/FolderContent.ml >> build/OCamlMake/Common/OCamlMake_Common_FolderContent.ml
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_FolderContent.mli ; cat OCamlMake/Common/FolderContent.mli >> build/OCamlMake/Common/OCamlMake_Common_FolderContent.mli
ocamlfind ocamlc -I build/OCamlMake/Common -I build/Utils -c build/OCamlMake/Common/OCamlMake_Common_FolderContent.mli
echo "module Timestamp = OCamlMake_Common_Timestamp module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Process.ml ; cat OCamlMake/Common/Process.ml >> build/OCamlMake/Common/OCamlMake_Common_Process.ml
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Process.mli ; cat OCamlMake/Common/Process.mli >> build/OCamlMake/Common/OCamlMake_Common_Process.mli
ocamlfind ocamlc -I build/OCamlMake/Common -I build/Utils -c build/OCamlMake/Common/OCamlMake_Common_Process.mli
echo "module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Timestamp.ml ; cat OCamlMake/Common/Timestamp.ml >> build/OCamlMake/Common/OCamlMake_Common_Timestamp.ml
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Timestamp.mli ; cat OCamlMake/Common/Timestamp.mli >> build/OCamlMake/Common/OCamlMake_Common_Timestamp.mli
ocamlfind ocamlc -I build/OCamlMake/Common -I build/Utils -c build/OCamlMake/Common/OCamlMake_Common_Timestamp.mli
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/Common -I build/Container -I build/Utils -package unix -package str -c build/OCamlMake/Common/OCamlMake_Common_Timestamp.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/Common -I build/Container -I build/Utils -package unix -package str -c build/OCamlMake/Common/OCamlMake_Common_Process.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/Common/OCamlMake_Common_FolderContent.ml
echo "module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/Common/OCamlMake_Common_Private.ml ; cat OCamlMake/Common/Private.ml >> build/OCamlMake/Common/OCamlMake_Common_Private.ml
cp OCamlMake/Common/Private.mli build/OCamlMake/Common/OCamlMake_Common_Private.mli
ocamlfind ocamlc -I build/OCamlMake/Common -I build/Utils -c build/OCamlMake/Common/OCamlMake_Common_Private.mli
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/Common -I build/Utils -package str -c build/OCamlMake/Common/OCamlMake_Common_Private.ml
echo "module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCamlMake_OCamlMake.ml ; cat OCamlMake/OCamlMake.ml >> build/OCamlMake/OCamlMake_OCamlMake.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/OCamlMake_OCamlMake.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/OCamlMake_CommonRules.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/OCamlMake_BootstrapRule.ml
mkdir build/OCamlMake/OCaml
echo "module Dependency = OCamlMake_OCaml_Dependency module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCaml/OCamlMake_OCaml_Flags.ml ; cat OCamlMake/OCaml/Flags.ml >> build/OCamlMake/OCaml/OCamlMake_OCaml_Flags.ml
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCaml/OCamlMake_OCaml_Flags.mli ; cat OCamlMake/OCaml/Flags.mli >> build/OCamlMake/OCaml/OCamlMake_OCaml_Flags.mli
ocamlfind ocamlc -I build/OCamlMake/OCaml -I build/OCamlMake/Common -I build/Utils -c build/OCamlMake/OCaml/OCamlMake_OCaml_Flags.mli
echo "module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end module OCamlDep = OCamlMake_OCaml_OCamlDep module Container = struct   module Slice = Container_Slice   module LinkedList = Container_LinkedList   module LruMap = Container_LruMap   module LinkedHashSet = Container_LinkedHashSet   module LinkedHashMap = Container_LinkedHashMap   module HashSet = Container_HashSet end module Canonical = OCamlMake_OCaml_Canonical" > build/OCamlMake/OCaml/OCamlMake_OCaml_Dependency.ml ; cat OCamlMake/OCaml/Dependency.ml >> build/OCamlMake/OCaml/OCamlMake_OCaml_Dependency.ml
echo "module Container : sig   module Slice = Container_Slice   module LinkedList = Container_LinkedList   module LruMap = Container_LruMap   module LinkedHashSet = Container_LinkedHashSet   module LinkedHashMap = Container_LinkedHashMap   module HashSet = Container_HashSet end module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCaml/OCamlMake_OCaml_Dependency.mli ; cat OCamlMake/OCaml/Dependency.mli >> build/OCamlMake/OCaml/OCamlMake_OCaml_Dependency.mli
ocamlfind ocamlc -I build/OCamlMake/OCaml -I build/Container -I build/Utils -c build/OCamlMake/OCaml/OCamlMake_OCaml_Dependency.mli
echo "module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end module OCamlDep = OCamlMake_OCaml_OCamlDep" > build/OCamlMake/OCaml/OCamlMake_OCaml_Canonical.ml ; cat OCamlMake/OCaml/Canonical.ml >> build/OCamlMake/OCaml/OCamlMake_OCaml_Canonical.ml
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCaml/OCamlMake_OCaml_Canonical.mli ; cat OCamlMake/OCaml/Canonical.mli >> build/OCamlMake/OCaml/OCamlMake_OCaml_Canonical.mli
ocamlfind ocamlc -I build/OCamlMake/OCaml -I build/OCamlMake -I build/Utils -c build/OCamlMake/OCaml/OCamlMake_OCaml_Canonical.mli
echo "module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDep.ml ; cat OCamlMake/OCaml/OCamlDep.ml >> build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDep.ml
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDep.mli ; cat OCamlMake/OCaml/OCamlDep.mli >> build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDep.mli
ocamlfind ocamlc -I build/OCamlMake/OCaml -I build/Utils -c build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDep.mli
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/OCaml -I build/Container -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDep.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/OCaml -I build/Container -I build/OCamlMake/Common -I build/OCamlMake -I build/Utils -package unix -package str -c build/OCamlMake/OCaml/OCamlMake_OCaml_Canonical.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/OCaml -I build/Container -I build/Utils -package unix -package str -c build/OCamlMake/OCaml/OCamlMake_OCaml_Dependency.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/OCaml -I build/Container -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/OCaml/OCamlMake_OCaml_Flags.ml
echo "module Dependency = OCamlMake_OCaml_Dependency module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module Flags = OCamlMake_OCaml_Flags module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end module Canonical = OCamlMake_OCaml_Canonical" > build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDocRules.ml ; cat OCamlMake/OCaml/OCamlDocRules.ml >> build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDocRules.ml
cp OCamlMake/OCaml/OCamlDocRules.mli build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDocRules.mli
ocamlfind ocamlc -I build/OCamlMake/OCaml -I build/OCamlMake -c build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDocRules.mli
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/OCaml -I build/OCamlMake -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDocRules.ml
echo "module Dependency = OCamlMake_OCaml_Dependency module OCamlMake_Common = struct   module Property = OCamlMake_Common_Property   module Private = OCamlMake_Common_Private   module Flag = OCamlMake_Common_Flag   module FolderContent = OCamlMake_Common_FolderContent   module Timestamp = OCamlMake_Common_Timestamp   module Process = OCamlMake_Common_Process end module Flags = OCamlMake_OCaml_Flags module Utils = struct   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end module Canonical = OCamlMake_OCaml_Canonical" > build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlRules.ml ; cat OCamlMake/OCaml/OCamlRules.ml >> build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlRules.ml
echo "module Utils : sig   module Predicate = Utils_Predicate   module Foldable = Utils_Foldable   module File = Utils_File   module Cache = Utils_Cache   module Iterable = Utils_Iterable   module Utils = Utils_Utils   module Log = Utils_Log   module Asserts = Utils_Asserts   module CommandLine = Utils_CommandLine end" > build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlRules.mli ; cat OCamlMake/OCaml/OCamlRules.mli >> build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlRules.mli
ocamlfind ocamlc -I build/OCamlMake/OCaml -I build/OCamlMake -I build/Utils -c build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlRules.mli
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake/OCaml -I build/OCamlMake -I build/OCamlMake/Common -I build/Utils -package unix -package str -c build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlRules.ml
ocamlfind ocamlc -strict-formats -strict-sequence -unsafe -noassert -I build/OCamlMake -I build/OCamlMake/Common -I build/OCamlMake/OCaml -I build/Utils -package unix -package str -c build/OCamlMake/OCamlMake_Make.ml
ocamlfind ocamlc -noassert -I build/OCamlMake -I build/OCamlMake/Common -I build/OCamlMake/OCaml -I build/Utils -linkpkg -package unix -package str -o build/OCamlMake/OCamlMake_Make.byte build/Utils/Utils_Iterable.cmo build/Utils/Utils_Utils.cmo build/Container/Container_HashSet.cmo build/Container/Container_LinkedHashMap.cmo build/Container/Container_LinkedHashSet.cmo build/Container/Container_LinkedList.cmo build/Container/Container_LruMap.cmo build/Utils/Utils_Log.cmo build/Utils/Utils_Foldable.cmo build/Container/Container_Slice.cmo build/Utils/Utils_File.cmo build/Utils/Utils_Asserts.cmo build/Utils/Utils_Cache.cmo build/Utils/Utils_CommandLine.cmo build/Utils/Utils_Predicate.cmo build/OCamlMake/Common/OCamlMake_Common_Property.cmo build/OCamlMake/Common/OCamlMake_Common_Flag.cmo build/OCamlMake/Common/OCamlMake_Common_Timestamp.cmo build/OCamlMake/Common/OCamlMake_Common_Process.cmo build/OCamlMake/Common/OCamlMake_Common_FolderContent.cmo build/OCamlMake/Common/OCamlMake_Common_Private.cmo build/OCamlMake/OCamlMake_OCamlMake.cmo build/OCamlMake/OCamlMake_CommonRules.cmo build/OCamlMake/OCamlMake_BootstrapRule.cmo build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDep.cmo build/OCamlMake/OCaml/OCamlMake_OCaml_Canonical.cmo build/OCamlMake/OCaml/OCamlMake_OCaml_Dependency.cmo build/OCamlMake/OCaml/OCamlMake_OCaml_Flags.cmo build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlDocRules.cmo build/OCamlMake/OCaml/OCamlMake_OCaml_OCamlRules.cmo build/OCamlMake/OCamlMake_Make.cmo
cp build/OCamlMake/OCamlMake_Make.byte OCamlMake/Make.byte
