module Any_error              = Any_error
module Async_heap             = Async_heap
module Async_mutex            = Async_mutex
module Coalesced_throttle     = Coalesced_throttle
module Color_print            = Async_color_print
module Command_rpc            = Command_rpc
  [@@deprecated "[since 2016-08] Use the standalone Command_rpc library instead"]
module Csv                    = Csv
module Deferred_cache         = Deferred_cache
module Deferred_list          = Deferred_list
module Delimited              = Delimited
module Embedded_script        = Embedded_script
module Enforcer               = Enforcer
module Fd_leak_check          = Fd_leak_check
module Find                   = Async_find
module Run_in_fork            = Run_in_fork
module Generator              = Generator
module Interactive            = Async_interactive
  [@@deprecated "[since 2016-08] Use the standalone Async_interactive library instead"]
module Keyed_sequencer        = Keyed_sequencer
module Log = struct
  include Async.Log
  include Extended_log
end
module Ltl                    = Ltl
module Mailbox                = Mailbox
module Pipe_iterator          = Pipe_iterator
module Pipe_zipper            = Pipe_zipper
module Priority_queue         = Priority_queue
module Process                = Process
module Reader = struct
  include Async.Reader
  include Reader_ext
end
module Semaphore              = Semaphore
module Shell                  = Async_shell
module Simple_tcp_proxy       = Simple_tcp_proxy
module Sound                  = Sound
module Udp                    = Udp
module Validate_command       = Validate_command
module Watcher                = Watcher
