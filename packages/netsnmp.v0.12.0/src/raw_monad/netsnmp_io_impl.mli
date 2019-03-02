type 'a t = 'a

include Io_intf.S with type 'a t := 'a t
