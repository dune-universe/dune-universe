(*
 *  This file is part of the gym-http-api OCaml binding project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

module Make (Client : Cohttp_lwt.S.Client) = struct
  open Lwt
  open Json_t

  (** {6. Utility functions} *)

  let parameters_of_json (o: json) : string =
    begin match o with
    | `Assoc [] -> ""
    | `Assoc ((x, v) :: l) ->
        let params = "?"^x^"="^(Yojson.Basic.to_string v) in
        List.fold_left
          (fun params (x, v) ->
             params^"&"^x^"="^(Yojson.Basic.to_string v))
          params l
    | _ ->
        Log.error "Rest" (Some "")
          ("parameters_of_json "^ (Yojson.Basic.pretty_to_string o) ^
           ": json object expected")
    end

  (** {6. Generic functions} *)

  let post base_url method_ req =
    let uri =
      Uri.of_string (base_url^method_)
    in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let data = ((Cohttp.Body.of_string req) :> Cohttp_lwt.Body.t) in
    let call =
      Client.post ~body:data ~headers uri >>= (fun (resp, body) ->
        let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        body |> Cohttp_lwt.Body.to_string >|= (fun body ->
          begin match code with
          | 200 | 201 | 204  -> body
          | _ ->
              Log.error
                "Rest" None
                (Format.sprintf "[POST %s] %d: %s" method_ code body)
          end))
    in
    let rsp = Lwt_main.run call in
    rsp

  let get base_url method_ params =
    let uri =
      Uri.of_string (base_url^method_^params)
    in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let call =
      Client.get ~headers uri >>= (fun (resp, body) ->
        let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        body |> Cohttp_lwt.Body.to_string >|= (fun body ->
          begin match code with
          | 200 -> body
          | _ ->
              Log.error
                "Rest" None
                (Format.sprintf "[GET %s] %d: %s" method_ code body)
          end))
    in
    let rsp = Lwt_main.run call in
    rsp


  let delete base_url method_ =
    let uri =
      Uri.of_string (base_url^method_)
    in
    let headers =
      let h = Cohttp.Header.init () in
      (* let h = Cohttp.Header.add_authorization h (`Basic (wcs_cred.cred_username, wcs_cred.cred_password)) in *)
      let h = Cohttp.Header.add h "Content-Type" "application/json" in
      h
    in
    let call =
      Client.delete ~headers uri >>= (fun (resp, body) ->
        let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        body |> Cohttp_lwt.Body.to_string >|= (fun body ->
          begin match code with
          | 200 | 201 -> body
          | _ ->
              Log.error
                "Rest" None
                (Format.sprintf "[DELETE %s] %d: %s" method_ code body)
          end))
    in
    let rsp = Lwt_main.run call in
    rsp

end
