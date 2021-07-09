let start_memprof_limits = Memprof_server.start_2

let set_global_memory_limit l = Limits_callbacks.global_limit := l

let limit_global_memory f =
  Memprof_server.check_memprof_limits_started "limit_global_memory" ;
  Limits_callbacks.limit_global_memory ~f

let limit_allocations ~limit f =
  Memprof_server.check_memprof_limits_started "limit_allocations" ;
  Limits_callbacks.limit_allocations ~limit ~f

let max_allocation_limit = Limits_callbacks.max_allocation_limit

let limit_with_token ~token f =
  Memprof_server.check_memprof_limits_started "limit_with_token" ;
  Limits_callbacks.limit_with_token ~token ~f

let is_interrupted = Limits_callbacks.is_interrupted

type 'a result = ('a, exn) Result.t

module Token = Token
module Masking = Masking
module Resource_bind = Resource_bind
module Memprof = Memprof
