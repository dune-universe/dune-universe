module Lwt_io_run = struct
  type 'a t = 'a Lwt.t
end

module Lwt_io_flush = struct
  type 'a t = 'a Lwt.t
  let return x = Lwt.return x
  let bind x ~f = Lwt.bind x f
  let to_run x = x
end

module Expect_test_config = struct
  module IO_run = Lwt_io_run
  module IO_flush = Lwt_io_flush
  let run x = Lwt_main.run (x ())
  let flush () = Lwt_io.flush_all ()
  let flushed () = Lwt_io.(buffered stdout = 0)
  let upon_unreleasable_issue = `CR
end
