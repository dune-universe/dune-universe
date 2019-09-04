RESET_MODULE(OCamlStandard)
  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/core.html *)
  #ifdef DEPRECATES_Pervasives
  module Pervasives = Stdlib
  #else
  ALIAS_STDLIB_MODULE(Pervasives)
  #endif

  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/stdlib.html *)
  ALIAS_STDLIB_MODULE(Arg)
  ALIAS_STDLIB_MODULE(Array)
  ALIAS_STDLIB_MODULE(ArrayLabels)
  #ifdef HAS_Bigarray
  ALIAS_STDLIB_MODULE(Bigarray)
  #endif
  ALIAS_STDLIB_MODULE(Buffer)
  ALIAS_STDLIB_MODULE(Bytes)
  ALIAS_STDLIB_MODULE(BytesLabels)
  ALIAS_STDLIB_MODULE(Callback)
  ALIAS_STDLIB_MODULE(Char)
  ALIAS_STDLIB_MODULE(Complex)
  ALIAS_STDLIB_MODULE(Digest)
  #ifdef HAS_Ephemeron
  ALIAS_STDLIB_MODULE(Ephemeron)
  #endif
  ALIAS_STDLIB_MODULE(Filename)
  #ifdef HAS_Float
  ALIAS_STDLIB_MODULE(Float)
  #endif
  ALIAS_STDLIB_MODULE(Format)
  ALIAS_STDLIB_MODULE(Gc)
  ALIAS_STDLIB_MODULE(Genlex)
  ALIAS_STDLIB_MODULE(Hashtbl)
  ALIAS_STDLIB_MODULE(Int32)
  ALIAS_STDLIB_MODULE(Int64)
  ALIAS_STDLIB_MODULE(Lazy)
  ALIAS_STDLIB_MODULE(Lexing)
  ALIAS_STDLIB_MODULE(List)
  ALIAS_STDLIB_MODULE(ListLabels)
  ALIAS_STDLIB_MODULE(Map)
  ALIAS_STDLIB_MODULE(Marshal)
  ALIAS_STDLIB_MODULE(MoreLabels)
  ALIAS_STDLIB_MODULE(Nativeint)
  ALIAS_STDLIB_MODULE(Oo)
  ALIAS_STDLIB_MODULE(Parsing)
  ALIAS_STDLIB_MODULE(Printexc)
  ALIAS_STDLIB_MODULE(Printf)
  ALIAS_STDLIB_MODULE(Queue)
  ALIAS_STDLIB_MODULE(Random)
  ALIAS_STDLIB_MODULE(Scanf)
  #ifdef HAS_Seq
  ALIAS_STDLIB_MODULE(Seq)
  #endif
  ALIAS_STDLIB_MODULE(Set)
  #ifdef HAS_Sort
  ALIAS_STDLIB_MODULE(Sort)
  #endif
  #ifdef HAS_Spacetime
  ALIAS_STDLIB_MODULE(Spacetime)
  #endif
  ALIAS_STDLIB_MODULE(Stack)
  ALIAS_STDLIB_MODULE(StdLabels)
  ALIAS_STDLIB_MODULE(Stream)
  ALIAS_STDLIB_MODULE(String)
  ALIAS_STDLIB_MODULE(StringLabels)
  ALIAS_STDLIB_MODULE(Sys)
  #ifdef HAS_Uchar
  ALIAS_STDLIB_MODULE(Uchar)
  #endif
  ALIAS_STDLIB_MODULE(Weak)

  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/parsing.html *)

  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libunix.html *)

  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libnum.html *)
  ALIAS_MODULE(Num)
  ALIAS_MODULE(Big_int)
  ALIAS_MODULE(Arith_status)

  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libstr.html *)
  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libthreads.html *)
  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libgraph.html *)
  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libdynlink.html *)
  (* https://caml.inria.fr/pub/docs/manual-ocaml-4.05/libbigarray.html *)
end

EMPTY_MODULE(Pervasives)

EMPTY_MODULE(Arg)
RESET_MODULE(Array)
  ALIAS_VALUE(get, 'a array -> int -> 'a, OCamlStandard.Array.get)
  ALIAS_VALUE(set, 'a array -> int -> 'a -> unit, OCamlStandard.Array.set)
end
EMPTY_MODULE(ArrayLabels)
#ifdef HAS_Bigarray
EMPTY_MODULE(Bigarray)
#endif
EMPTY_MODULE(Buffer)
EMPTY_MODULE(Bytes)
EMPTY_MODULE(BytesLabels)
EMPTY_MODULE(Callback)
EMPTY_MODULE(Char)
EMPTY_MODULE(Complex)
EMPTY_MODULE(Digest)
#ifdef HAS_Ephemeron
EMPTY_MODULE(Ephemeron)
#endif
EMPTY_MODULE(Filename)
#ifdef HAS_Float
EMPTY_MODULE(Float)
#endif
EMPTY_MODULE(Format)
EMPTY_MODULE(Gc)
EMPTY_MODULE(Genlex)
EMPTY_MODULE(Hashtbl)
EMPTY_MODULE(Int32)
EMPTY_MODULE(Int64)
EMPTY_MODULE(Lazy)
EMPTY_MODULE(Lexing)
EMPTY_MODULE(List)
EMPTY_MODULE(ListLabels)
EMPTY_MODULE(Map)
EMPTY_MODULE(Marshal)
EMPTY_MODULE(MoreLabels)
EMPTY_MODULE(Nativeint)
EMPTY_MODULE(Oo)
EMPTY_MODULE(Parsing)
EMPTY_MODULE(Printexc)
EMPTY_MODULE(Printf)
EMPTY_MODULE(Queue)
EMPTY_MODULE(Random)
EMPTY_MODULE(Scanf)
#ifdef HAS_Seq
EMPTY_MODULE(Seq)
#endif
EMPTY_MODULE(Set)
EMPTY_MODULE(Sort)
#ifdef HAS_Spacetime
EMPTY_MODULE(Spacetime)
#endif
EMPTY_MODULE(Stack)
EMPTY_MODULE(StdLabels)
EMPTY_MODULE(Stream)
RESET_MODULE(String)
  ALIAS_VALUE(get, string -> int -> char, OCamlStandard.String.get)
  ALIAS_VALUE(set, bytes -> int -> char -> unit, OCamlStandard.Bytes.set)
end
EMPTY_MODULE(StringLabels)
EMPTY_MODULE(Sys)
#ifdef HAS_Uchar
EMPTY_MODULE(Uchar)
#endif
EMPTY_MODULE(Weak)

EMPTY_MODULE(Num)
EMPTY_MODULE(Big_int)
EMPTY_MODULE(Arith_status)
