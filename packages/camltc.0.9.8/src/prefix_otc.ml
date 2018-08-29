(*
Copyright (2010-2014) INCUBAID BVBA

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

open Otc

module Prefix_otc = struct

  type t = Bdb.bdb

  let get bdb prefix key =
    Lwt.catch (fun () ->
    let value = Bdb.get bdb (prefix ^ key) in
    (*let _ = log "GET %d" (String.length value) in*)
      Lwt.return value)
      (fun e -> Lwt.fail e)


  let put bdb prefix key value =
    (*let _ = log "PUT %d" (String.length value) in*)
    Lwt.catch (fun () ->
      let () = Bdb.put bdb (prefix ^ key) value in
      Lwt.return ()
    ) (fun e -> Lwt.fail e)

  let out bdb prefix key =
    Lwt.catch (fun () ->
      let () = Bdb.out bdb (prefix ^ key) in
      Lwt.return ()
    ) (fun e -> Lwt.fail e)

  let fold (f:string -> string -> 'c -> 'c) (bdb:t) (prefix:string) (init:'c) =
    Lwt.catch (fun () ->
      let f' a key = f key (Bdb.get bdb key) a in
      let (keys:string array) = Bdb.prefix_keys bdb prefix (-1) in
      let x = Array.fold_left f' init keys in
      Lwt.return x
    ) (fun e -> Lwt.fail e)

  let iter (f:string -> string -> unit) (bdb:t) (prefix:string) =
    fold (fun k v _ -> f k v) bdb prefix ()

  let all_keys bdb prefix =
    fold (fun k v init -> k::init) bdb prefix []

  let all_values bdb prefix =
    fold (fun k v init -> v::init) bdb prefix []

      (* TODO: add more prefixed Otc methods as needed *)

end
