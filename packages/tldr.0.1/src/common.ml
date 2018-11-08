open Core
open Lwt    
open Cohttp    
open Cohttp_lwt_unix

type t =
  | Error of string
  | Success of string
  | Missing
  
module Environment = struct
  let system =
    match Sys.os_type with
    | "Unix" ->
      let ic = Unix.open_process_in "uname" in
      let uname = In_channel.input_line ic in
      let () = In_channel.close ic in
      (match uname with
        | Some  "Darwin" -> "osx"
        | _  -> "linux")
    | "Win32" -> "windows"
    | _ -> "common"
end

module Cache = struct
  let download_location = "https://tldr-pages.github.io/assets/tldr.zip"
  
  let use_cache =
    Sys.getenv "TLDR_CACHE_ENABLED"
    |> (Option.value_map ~default:true ~f:(function "1" -> true | _ -> false))

  let max_age =
    Sys.getenv "TLDR_MAX_CACHE_AGE"
    |> Option.value_map ~default:24. ~f:Float.of_string

  let directory =
    let open Filename in
    match Sys.getenv "XDG_CACHE_HOME" with
    | Some path -> concat path "tldr"
    | None ->
      match Sys.getenv "HOME" with
      | Some path -> concat (concat path ".cache") "tldr"
      | None      -> concat (concat "~"  ".cache") "tldr"
  (* I con't know if this actually works *)

  let get_file_path command platform =
    Filename.concat directory (command ^ "_" ^ platform ^ ".md")

  let load_page command platform =
    let file = get_file_path command platform in
    let exists = file |> Fpath.v |> Bos.OS.File.exists in
    
    if use_cache && exists = Result.Ok true then
      let last_modified = (Unix.stat file).st_mtime 
      and cache_epoch = max_age *. 60. *. 60. *. 24. 
      and cur_time = Unix.time () in
      if cur_time -. last_modified > cache_epoch then
        Missing
      else
        Success (In_channel.read_all file)
    else
      Missing

  let store_page page command platform =
    let file_path = (get_file_path command platform) in
    if directory |> Fpath.v |> Bos.OS.File.exists = Result.Ok false then
      ignore (Bos.OS.Dir.create (directory |> Fpath.v));
    Out_channel.write_all file_path ~data:page
end


module Remote = struct
  let default_remote = "https://raw.githubusercontent.com/tldr-pages/tldr/master/pages"

  let get_page_url ?(remote = default_remote) ?(platform = Environment.system) command =
      remote ^ "/" ^ platform ^ "/" ^ command ^ ".md"

  let get_page ?(remote = default_remote) ?(platform = Environment.system) command =
    let url = get_page_url ~remote:remote ~platform:platform command in
    let request = Client.get (Uri.of_string url) >>= fun (resp , body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      match code with
      | 200 -> Success body
      | 404 -> Missing
      | _   -> Error "There was an error with connection"
    in
    Lwt_main.run request
end

let (<|>) first second =
  match first with
  | Missing -> Lazy.force second
  | x -> x


let get_page command platform =
  Cache.load_page command "common"
  <|> lazy (Cache.load_page command platform)
  <|> lazy (Remote.get_page command ~platform:"common")
  <|> lazy (Remote.get_page command ~platform:platform)
  <|> lazy (Missing)

