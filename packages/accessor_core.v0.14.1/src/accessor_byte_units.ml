open! Core_kernel
open! Import

let bytes =
  [%accessor
    Accessor.isomorphism ~get:Byte_units.bytes_int_exn ~construct:Byte_units.of_bytes_int]
;;

let kilobytes =
  [%accessor
    Accessor.isomorphism ~get:Byte_units.kilobytes ~construct:Byte_units.of_kilobytes]
;;

let megabytes =
  [%accessor
    Accessor.isomorphism ~get:Byte_units.megabytes ~construct:Byte_units.of_megabytes]
;;

let gigabytes =
  [%accessor
    Accessor.isomorphism ~get:Byte_units.gigabytes ~construct:Byte_units.of_gigabytes]
;;

let terabytes =
  [%accessor
    Accessor.isomorphism ~get:Byte_units.terabytes ~construct:Byte_units.of_terabytes]
;;

let petabytes =
  [%accessor
    Accessor.isomorphism ~get:Byte_units.petabytes ~construct:Byte_units.of_petabytes]
;;

let exabytes =
  [%accessor
    Accessor.isomorphism ~get:Byte_units.exabytes ~construct:Byte_units.of_exabytes]
;;

let words =
  [%accessor
    Accessor.isomorphism
      ~get:Byte_units.words_float
      ~construct:Byte_units.of_words_float_exn]
;;
