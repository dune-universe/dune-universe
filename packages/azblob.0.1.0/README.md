# Azblob -- A trivial Azure Blob Storage interface for OCaml

## Usage

```
open! Core
open! Async
open! Cohttp
open! Cohttp_async

let example () =
  let conn = Azblob.Conn.parse_exn "<your Blob Storage connection string here>" in
  let headers = Header.of_list ["content-type", "text/plain"] in
  let path = "/your-container/your-blob.txt" in
  let body = Body.of_string "Hello, world.\n" in
  Azblob_async.put_blob conn ~headers ~path ~body
  >>= fun (_res, _body) ->
  ()
```
