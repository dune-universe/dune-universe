module type V4 = sig
  include Mirage_stack.V4
  with type 'a io = 'a Lwt.t
   and type ipv4addr = Ipaddr.V4.t
   and type buffer = Cstruct.t
end
