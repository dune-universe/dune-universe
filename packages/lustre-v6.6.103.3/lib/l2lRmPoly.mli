(* Time-stamp: <modified the 30/06/2014 (at 10:26) by Erwan Jahier> *)

(** Remove overloading of nodes used with iterators.

nb: it actually does not remove remove polymorphism actually -> TODO: Rename this module.

nb2 : only if/then/else is truely polymorphic.



nb :
- il est préférable d'appeler
  ce module AVANT L2lAliasType,
  sinon on risque d'avoir des alias bizarres, du style :
  'anynum_4_7_int'
  au lieu de
  'int_4_7'
  Mais bon, normalement c'est quand même correct ...
*)

val doit : LicPrg.t -> LicPrg.t
