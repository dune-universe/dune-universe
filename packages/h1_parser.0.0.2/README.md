# H1

h1 is a simple http 1.1 implementation. The core protocol layout doesn't enforce any concurrency strategry
and as a result it is possible to provide small translation modules that can provide an Lwt or Async aware interface.


### Warning
This is currently a proof-of-concept. There are many rough edges and probably many bugs. Use it at your own risk.
That said, the approach seems to work well and in my initial tests the performance seems pretty decent. With some more effort it should be possible to get to a respectable state with extensive tests.

## Features/Goals
* Asynchronous pull based design
* No I/O in the core library. Use it with any I/O paradigm (async, sync, threaded, etc)

## Usage

### HTTP Server

Example using async:

<!-- $MDX file=example/main_async.ml,part=simple_server -->
```ocaml
let run (sock : Fd.t) =
  let service (req, _body) =
    let target = Request.path req in
    let resp =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Content-Length", Int.to_string (Base_bigstring.length text)) ])
        `Ok
    in
    match target with
    | "/delay" ->
        let%map () = after Time.Span.millisecond in
        (resp, `Bigstring text)
    | _ -> return (resp, `Bigstring text)
  in
  let conn =
    H1_async.create sock ~read_buffer_size:(10 * 1024)
      ~write_buffer_size:(10 * 1024)
  in
  H1_async.run conn service
```

## Todo
- [x] Chunked Encoding
- [ ] 0 copy streaming for bodies
- [ ] Better error handling
- [ ] Add docs
- [ ] Http client implementation (This can be pushed till after a full working/tested/documented server implementation)
