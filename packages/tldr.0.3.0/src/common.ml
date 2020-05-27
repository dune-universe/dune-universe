open Base

type t =
  | Error of string
  | Success of string
  | Missing

module Environment = struct
  let system =
    match Sys.os_type with
    | "Unix" ->
       let ic = Unix.open_process_in "uname" in
       let uname = Stdio.In_channel.input_line_exn ic in
       ignore (Unix.close_process_in ic : Unix.process_status);
       (match uname with
        | "Darwin" -> "osx"
        | _  -> "linux")
    | "Win32" -> "windows"
    | _ -> "common"
end

module Cache = struct
  open Stdlib.Filename
  let download_location = "https://tldr-pages.github.io/assets/tldr.zip"

  let use_cache =
    Sys.getenv "TLDR_CACHE_ENABLED"
    |> function Some "0" -> false | _ -> true

  let max_age =
    Sys.getenv "TLDR_MAX_CACHE_AGE"
    |> Option.value_map ~f:Float.of_string ~default:24.

  let directory =
    match Sys.getenv "XDG_CACHE_HOME" with
    | Some path -> concat path "tldr"
    | None ->
      match Sys.getenv "HOME" with
      | Some path -> concat (concat path ".cache") "tldr"
      | None      -> concat (concat "~"  ".cache") "tldr"
  (* I don't know if this actually works *)

  let get_file_path command platform =
    if not (Caml.Sys.file_exists directory) then
      Unix.mkdir directory 0o755;
    concat directory (String.concat ~sep:"" [command; "_"; platform; ".md"])

  let read_all filename =
    Stdio.In_channel.read_all filename

  let load_page command platform =
    let file = get_file_path command platform in
    let exists = Caml.Sys.file_exists file in

    if use_cache && exists then
      let last_modified = (Unix.stat file).st_mtime
      and cache_epoch = max_age *. 60. *. 60. *. 24.
      and cur_time = Unix.time () in
      if Poly.(cur_time -. last_modified > cache_epoch) then
        Missing
      else
        Success (read_all file)
    else
      Missing

  let store_page page command platform =
    let open Stdio.Out_channel in
    let file_path = (get_file_path command platform) in
    let oc = create file_path in
    fprintf oc "%s" page;
    flush oc;
    close oc;
end


module Remote = struct
  open Lwt
  open Cohttp
  open Cohttp_lwt_unix

  let default_remote = "https://raw.githubusercontent.com/tldr-pages/tldr/master/pages"

  let get_page_url ?(remote = default_remote) ?(platform = Environment.system) command =
    String.concat ~sep:"" [remote; "/"; platform; "/"; command; ".md"]

  let get_page ?(remote = default_remote) ?(platform = Environment.system) command =
    let url = get_page_url ~remote ~platform command in
    let request =
      Client.get (Uri.of_string url) >>= fun (resp, body) ->
      let code             = resp |> Response.status |> Code.code_of_status in
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      match code with
      | 200 -> if Cache.use_cache then
                 Cache.store_page body command platform;
               Success body
      | 404 -> Missing
      | _   -> Error "There was an error with connection"
    in
    Lwt_main.run request
end

let (<|>) first second =
  match first with
  | Missing -> Lazy.force second
  | x       -> x


let get_page command platform =
  Cache.load_page command "common"
  <|> lazy (Cache.load_page command platform)
  <|> lazy (Remote.get_page command ~platform:"common")
  <|> lazy (Remote.get_page command ~platform)
