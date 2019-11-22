open Netsnmp_raw_monad

include Netsnmp_intf.S with module IO := Netsnmp_io_impl
