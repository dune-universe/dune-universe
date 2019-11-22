open Netsnmp_raw_monad

module type IO = Io_intf.S

module Netsnmp(IO : IO) : Netsnmp_intf.S with module IO := IO
