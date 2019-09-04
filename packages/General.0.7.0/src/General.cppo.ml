module Reset = struct
  #include "Reset/CommonHeader.ml"
  #include "Reset/DefinitionHeader.ml"

  module ResetPervasives = struct
    #include "Reset/ResetPervasives.ml"
  end

  module ResetStandardLibrary = struct
    #include "Reset/ResetStandardLibrary.ml"
  end

  #include "Reset/Footer.ml"
end

open Reset.ResetPervasives
open Reset.ResetStandardLibrary

module OCSP = OCamlStandard.Pervasives

module Equate = struct
  #include "Equate.ml"
end

module Compare = struct
  #include "Compare.ml"
end

module Shorten = struct
  #include "Shorten.ml"
end

module Foundations = struct
  module Format = struct
    #include "Foundations/Format.ml"
  end

  module Lazy = struct
    #include "Foundations/Lazy.ml"
  end

  module Exception = struct
    #include "Foundations/Exception.ml"
  end

  module Functions = struct
    #include "Foundations/Functions.ml"
  end

  module Int = struct
    #include "Foundations/Int.ml"
  end

  module Bool = struct
    #include "Foundations/Bool.ml"
  end

  module Option = struct
    #include "Foundations/Option.ml"
  end

  module List = struct
    #include "Foundations/List.ml"
  end

  module CallStack = struct
    #include "Foundations/CallStack.ml"
  end

  module Float = struct
    #include "Foundations/Float.ml"
  end

  module Reference = struct
    #include "Foundations/Reference.ml"
  end

  module String = struct
    #include "Foundations/String.ml"
  end

  module IntRange = struct
    #include "Foundations/IntRange.ml"
  end

  module Tuples = struct
    #include "Foundations/Tuples.ml"
  end

  module Exit = struct
    #include "Foundations/Exit.ml"
  end

  module Stream = struct
    #include "Foundations/Stream.ml"
  end

  module Unit = struct
    #include "Foundations/Unit.ml"
  end

  module PervasivesWhitelist = struct
    #include "Foundations/PervasivesWhitelist.ml"
  end
end

module Ubiquitous = struct
  include Reset.ResetPervasives
  include Reset.ResetStandardLibrary
  include Foundations.PervasivesWhitelist
end

module Pervasives = Ubiquitous

open Ubiquitous

open Foundations

module Testing = struct
  #include "Testing.ml"
end

module Traits = struct
  module Representable = struct
    #include "Traits/Representable.ml"
  end

  module Equatable = struct
    #include "Traits/Equatable.ml"
  end

  module Comparable = struct
    #include "Traits/Comparable.ml"
  end

  module Displayable = struct
    #include "Traits/Displayable.ml"
  end

  module Parsable = struct
    #include "Traits/Parsable.ml"
  end

  module PredSucc = struct
    #include "Traits/PredSucc.ml"
  end

  module Ringoid = struct
    #include "Traits/Ringoid.ml"
  end

  module FilterMapable = struct
    #include "Traits/FilterMapable.ml"
  end

  module Foldable = struct
    #include "Traits/Foldable.ml"
  end

  module Scanable = struct
    #include "Traits/Scanable.ml"
  end
end

module Concepts = struct
  module Identifiable = struct
    #include "Concepts/Identifiable.ml"
  end

  module Able = struct
    #include "Concepts/Able.ml"
  end

  module Number = struct
    #include "Concepts/Number.ml"
  end

  module RealNumber = struct
    #include "Concepts/RealNumber.ml"
  end

  module Integer = struct
    #include "Concepts/Integer.ml"
  end
end

module Array = struct
  #include "Implementation/Array.ml"
end

module Exception = struct
  #include "Implementation/Exception.ml"
end

module Format = struct
  #include "Implementation/Format.ml"
end

module StandardInt = struct
  #include "Implementation/StandardInt.ml"
end

module Int32 = struct
  #include "Implementation/Int32.ml"
end

module Int64 = struct
  #include "Implementation/Int64.ml"
end

module Float = struct
  #include "Implementation/Float.ml"
end

module BigInt = struct
  #include "Implementation/BigInt.ml"
end

module Bool = struct
  #include "Implementation/Bool.ml"
end

module Bytes = struct
  #include "Implementation/Bytes.ml"
end

module Int = struct
  #include "Implementation/Int.ml"
end

module List = struct
  #include "Implementation/List.ml"
end

module CallStack = struct
  #include "Implementation/CallStack.ml"
end

module Char = struct
  #include "Implementation/Char.ml"
end

module Exit = struct
  #include "Implementation/Exit.ml"
end

module Functions = struct
  #include "Implementation/Functions.ml"
end
module Function1 = Functions.Function1
module Function2 = Functions.Function2
module Function3 = Functions.Function3
module Function4 = Functions.Function4
module Function5 = Functions.Function5

module InChannel = struct
  #include "Implementation/InChannel.ml"
end

module InFile = struct
  #include "Implementation/InFile.ml"
end

module IntRange = struct
  #include "Implementation/IntRange.ml"
end

module Lazy = struct
  #include "Implementation/Lazy.ml"
end

module NativeInt = struct
  #include "Implementation/NativeInt.ml"
end

module Option = struct
  #include "Implementation/Option.ml"
end

module OutChannel = struct
  #include "Implementation/OutChannel.ml"
end

module OutFile = struct
  #include "Implementation/OutFile.ml"
end

module Tuples = struct
  #include "Implementation/Tuples.ml"
end
module Tuple2 = Tuples.Tuple2
module Tuple3 = Tuples.Tuple3
module Tuple4 = Tuples.Tuple4
module Tuple5 = Tuples.Tuple5

module RedBlackTree = struct
  #include "Implementation/RedBlackTree.ml"
end

module BinaryHeap = struct
  #include "Implementation/BinaryHeap.ml"
end

module PriorityQueue = struct
  #include "Implementation/PriorityQueue.ml"
end

module Reference = struct
  #include "Implementation/Reference.ml"
end

module SortedMap = struct
  #include "Implementation/SortedMap.ml"
end

module SortedSet = struct
  #include "Implementation/SortedSet.ml"
end

module Heap = struct
  #include "Implementation/Heap.ml"
end

module StandardOutChannel = struct
  #include "Implementation/StandardOutChannel.ml"
end

module StdErr = struct
  #include "Implementation/StdErr.ml"
end

module StdIn = struct
  #include "Implementation/StdIn.ml"
end

module StdOut = struct
  #include "Implementation/StdOut.ml"
end

module Stream = struct
  #include "Implementation/Stream.ml"
end

module String = struct
  #include "Implementation/String.ml"
end

module TestingTests = struct
  #include "Implementation/TestingTests.ml"
end

module Unit = struct
  #include "Implementation/Unit.ml"
end

module Specializations = struct
  module List = struct
    #include "Specializations/List.ml"
  end

  module Option = struct
    #include "Specializations/Option.ml"
  end

  module Reference = struct
    #include "Specializations/Reference.ml"
  end

  module SortedMap = struct
    #include "Specializations/SortedMap.ml"
  end

  module SortedSet = struct
    #include "Specializations/SortedSet.ml"
  end
end

module FloatOption = Specializations.Option.Float
module IntOption = Specializations.Option.Int
module StringOption = Specializations.Option.String

module FloatReference = Specializations.Reference.Float
module IntReference = Specializations.Reference.Int
module StringReference = Specializations.Reference.String

module FloatList = Specializations.List.Float
module IntList = Specializations.List.Int
module StringList = Specializations.List.String

module CharSortedSet = Specializations.SortedSet.Char
module FloatSortedSet = Specializations.SortedSet.Float
module IntSortedSet = Specializations.SortedSet.Int
module StringSortedSet = Specializations.SortedSet.String

module CharSortedMap = Specializations.SortedMap.Char
module FloatSortedMap = Specializations.SortedMap.Float
module IntSortedMap = Specializations.SortedMap.Int
module StringSortedMap = Specializations.SortedMap.String

module Standard = struct
  module Testing = Testing

  module Array = Array
  module BigInt = BigInt
  module Bool = Bool
  module Bytes = Bytes
  module CallStack = CallStack
  module Char = Char
  module Exception = Exception
  module Exit = Exit
  module Float = Float
  module Format = Format
  module Function1 = Function1
  module Function2 = Function2
  module Function3 = Function3
  module Function4 = Function4
  module Function5 = Function5
  module Heap = Heap
  module InChannel = InChannel
  module InFile = InFile
  module Int = Int
  module Int32 = Int32
  module Int64 = Int64
  module Lazy = Lazy
  module List = List
  module NativeInt = NativeInt
  module Option = Option
  module OutChannel = OutChannel
  module OutFile = OutFile
  module PriorityQueue = PriorityQueue
  module Reference = Reference
  module SortedMap = SortedMap
  module SortedSet = SortedSet
  module StdErr = StdErr
  module StdIn = StdIn
  module StdOut = StdOut
  module Stream = Stream
  module String = String
  module Tuple2 = Tuple2
  module Tuple3 = Tuple3
  module Tuple4 = Tuple4
  module Tuple5 = Tuple5
  module Unit = Unit

  module IntRange = IntRange

  module FloatOption = FloatOption
  module IntOption = IntOption
  module StringOption = StringOption

  module FloatReference = FloatReference
  module IntReference = IntReference
  module StringReference = StringReference

  module FloatList = FloatList
  module IntList = IntList
  module StringList = StringList

  module CharSortedSet = CharSortedSet
  module FloatSortedSet = FloatSortedSet
  module IntSortedSet = IntSortedSet
  module StringSortedSet = StringSortedSet

  module CharSortedMap = CharSortedMap
  module FloatSortedMap = FloatSortedMap
  module IntSortedMap = IntSortedMap
  module StringSortedMap = StringSortedMap

  include (
    Ubiquitous: module type of Ubiquitous[@remove_aliases]
    with module Array := Array
    and module Bytes := Bytes
    and module Char := Char
    #ifdef HAS_Float
    and module Float := Float
    #endif
    and module Format := Format
    and module Int32 := Int32
    and module Int64 := Int64
    and module Lazy := Lazy
    and module List := List
    and module Stream := Stream
    and module String := String
  )
end

module Abbr = struct
  module Tst = Testing

  module Ar = Array
  module BigInt = BigInt
  module Bo = Bool
  module By = Bytes
  module CallStack = CallStack
  module Ch = Char
  module Exit = Exit
  module Exn = Exception
  module Fl = Float
  module Frmt = Format
  module Fun1 = Function1
  module Fun2 = Function2
  module Fun3 = Function3
  module Fun4 = Function4
  module Fun5 = Function5
  module Heap = Heap
  module InCh = InChannel
  module InFile = InFile
  module Int = Int
  module Int32 = Int32
  module Int64 = Int64
  module Laz = Lazy
  module Li = List
  module NativeInt = NativeInt
  module Opt = Option
  module OutCh = OutChannel
  module OutFile = OutFile
  module PriQu = PriorityQueue
  module Ref = Reference
  module SoMap = SortedMap
  module SoSet = SortedSet
  module StdErr = StdErr
  module StdIn = StdIn
  module StdOut = StdOut
  module Str = String
  module Strm = Stream
  module Tu2 = Tuple2
  module Tu3 = Tuple3
  module Tu4 = Tuple4
  module Tu5 = Tuple5
  module Unit = Unit

  module IntRa = IntRange

  module FlOpt = FloatOption
  module IntOpt = IntOption
  module StrOpt = StringOption

  module FlRef = FloatReference
  module IntRef = IntReference
  module StrRef = StringReference

  module FlLi = FloatList
  module IntLi = IntList
  module StrLi = StringList

  module ChSoSet = CharSortedSet
  module FlSoSet = FloatSortedSet
  module IntSoSet = IntSortedSet
  module StrSoSet = StringSortedSet

  module ChSoMap = CharSortedMap
  module FlSoMap = FloatSortedMap
  module IntSoMap = IntSortedMap
  module StrSoMap = StringSortedMap

  include (
    Ubiquitous: module type of Ubiquitous[@remove_aliases]
    with module Int32 := Int32
    and module Int64 := Int64
  )
end

module Tests = struct
  open Testing

  let test = "General" >:: [
    BigInt.Tests.test;
    BinaryHeap.Tests.test;
    Bool.Tests.test;
    CallStack.Tests.test;
    Exception.Tests.test;
    Float.Tests.test;
    Functions.Tests.test;
    Int.Tests.test;
    Int32.Tests.test;
    Int64.Tests.test;
    Lazy.Tests.test;
    List.Tests.test;
    NativeInt.Tests.test;
    Option.Tests.test;
    RedBlackTree.Tests.test;
    Stream.Tests.test;
    String.Tests.test;
    Tuples.Tests.test;

    IntRange.Tests.test;

    TestingTests.Tests.test;

    "Syntactic sugar" >:: [
      "Standard" >:: Standard.[
        "array" >:: [
          "get" >: (let a: int array = [|42|] in lazy (check_int ~expected:42 a.(0)));
          "set" >: (let a: int array = [|42|] in lazy (check_int ~expected:42 (Array.get a 0); a.(0) <- 37; check_int ~expected:37 (Array.get a 0)));
        ];
        (* @todo Use check_char *)
        "string" >:: [
          "get" >: (let a: string = "a" in lazy (Char.(check ~repr ~equal ~expected:'a' a.[0])));
          #ifdef STRINGS_ARE_MUTABLE
          (* @todo Fix that test in node.js *)
          (* "set" >: (let a: string = "a" in lazy (Char.(check ~repr ~equal ~expected:'a' (String.get a 0)); a.[0] <- 'z'; Char.(check ~repr ~equal ~expected:'z' (String.get a 0)))); *)
          #endif
        ];
        "bytes" >:: [
          "set" >: (let a: bytes = Bytes.of_string "a" in lazy (Char.(check ~repr ~equal ~expected:'a' (Bytes.get a 0)); a.[0] <- 'z'; Char.(check ~repr ~equal ~expected:'z' (Bytes.get a 0))));
        ];
      ];
      "Abbr" >:: Abbr.[
        "array" >:: [
          "get" >: (let a: int array = [|42|] in lazy (check_int ~expected:42 a.(0)));
          "set" >: (let a: int array = [|42|] in lazy (check_int ~expected:42 (Ar.get a 0); a.(0) <- 37; check_int ~expected:37 (Ar.get a 0)));
        ];
        (* @todo Use check_char *)
        "string" >:: [
          "get" >: (let a: string = "a" in lazy (Ch.(check ~repr ~equal ~expected:'a' a.[0])));
          #ifdef STRINGS_ARE_MUTABLE
          (* "set" >: (let a: string = "a" in lazy (Ch.(check ~repr ~equal ~expected:'a' (Str.get a 0)); a.[0] <- 'z'; Ch.(check ~repr ~equal ~expected:'z' (Str.get a 0)))); *)
          #endif
        ];
        "bytes" >:: [
          "set" >: (let a: bytes = By.of_string "a" in lazy (Ch.(check ~repr ~equal ~expected:'a' (By.get a 0)); a.[0] <- 'z'; Ch.(check ~repr ~equal ~expected:'z' (By.get a 0))));
        ];
      ];
    ];
  ]
end
