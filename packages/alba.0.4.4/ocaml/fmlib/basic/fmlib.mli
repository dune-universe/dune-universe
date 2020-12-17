(** Fmlib: Functional Monadic Library *)





(** {1 Parsing }*)

module Position         = Position
module Character_parser = Character_parser
module Generic_parser   = Generic_parser







(** {1 Pretty Printing }*)

module Pretty_print = Pretty_print
module Pretty_printer = Pretty_printer





(** {1 Applications } *)

(** {2 Console Applications }*)

module Io = Io

module Make_io = Make_io

module String_printer = String_printer



(** {2 Web Applications} *)

module Web_application = Web_application







(** {1 Basic Functions} *)


val identity: 'a -> 'a
(** Identity function *)





(** {1 Module Types} *)

module Module_types  = Module_types





(** {1 Standard Data Types} *)

module Array = Array

module List = List

module Deque = Deque

module Finite_map = Finite_map

module Monad = Monad

module Option = Option

module Pool = Pool

module Red_black = Red_black

module Readable_printer = Readable_printer

module Result = Result

module Sequence = Sequence

module Vector = Vector


module Common = Common







(** {1 Old modules (deprecated)} *)

module Argument_parser = Argument_parser
