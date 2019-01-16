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

open Gym_j
open Gym_t
open Json_t

(** Server url: default is [http://127.0.0.1:5000] *)
let base_url = ref "http://127.0.0.1:5000"

(** [env_create env_id] creates an instance of the specified
    environment (e.g., ["CartPole-v0"]). It returns the instance
    identifier.
*)
let env_create : string -> instance_id = begin
  fun env_id ->
    let method_ = "/v1/envs/" in
    let req = string_of_env_id { env_id = env_id; } in
    let rsp = Rest.post !base_url method_ req in
    instance_id_of_string rsp
end

(** [env_list_all ()] lists all the environments running on the server
    as a pair [(instance_id, env_id)] (e.g. [[("3c657dbc", "CartPole-v0")]]).
*)
let env_list_all : unit -> (instance_id * string) list = begin
  fun () ->
    let method_ = "/v1/envs/" in
    let params = "" in
    let rsp = Rest.get !base_url method_ params in
    List.map
      (fun (instance_id, env_id) -> ({instance_id = instance_id}, env_id))
      (all_envs_of_string rsp).all_envs
end

(** [env_reset instance_id] resets the state of the environment and
    return an initial observation.
*)
let env_reset : instance_id -> observation = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/reset/" in
    let req = "" in
    let rsp = Rest.post !base_url method_ req in
    observation_of_string rsp
end

(** [env_step instance_id action render] steps though an environment
    using an action. If [render] is true, a graphical feedback if
    display by the server.
*)
let env_step : instance_id -> action -> bool -> step_response = begin
  fun instance_id action render ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/step/" in
    let req =
      string_of_step_param { step_render = render;
                             step_action = `Int action.action; }
    in
    let rsp = Rest.post !base_url method_ req in
    step_response_of_string rsp
end

(** [env_action_space_info instance_id] gets information (name and
    dimensions/bounds) of the env's action_space.
*)
let env_action_space_info : instance_id -> action_space_info = begin
  fun instance_id  ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/action_space/" in
    let params = "" in
    let rsp = Rest.get !base_url method_ params in
    action_space_info_of_string rsp
end

(** [env_action_space_sample instance_id] samples randomly from the
    env's action_space.
*)
let env_action_space_sample : instance_id -> action = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/action_space/sample" in
    let params = "" in
    let rsp = Rest.get !base_url method_ params in
    action_of_string rsp
end

(** [env_action_space_contains instance_id x] checks to see if the
    value [x] is valid in the env's action_space.
*)
let env_action_space_contains : instance_id -> int -> bool = begin
  fun instance_id x ->
    let method_ =
      "/v1/envs/"^instance_id.instance_id^"/action_space/contains/"^(string_of_int x)
    in
    let params = "" in
    let rsp = Rest.get !base_url method_ params in
    (action_space_contains_response_of_string rsp).action_space_contains_member
end

(** [env_observation_space_info instance_id] gets information (name
    and dimensions/bounds) of the env's observation_space.
*)
let env_observation_space_info : instance_id -> observation_space_info = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/observation_space/" in
    let params = "" in
    let rsp = Rest.get !base_url method_ params in
    observation_space_info_of_string rsp
end

(** [env_observation_space_contains instance_id params] assesses that
    the parameters are members of the env's observation_space.
*)
let env_observation_space_contains : instance_id -> json -> bool = begin
  fun instance_id params ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/observation_space/contains" in
    let req = string_of_json params in
    let rsp = Rest.post !base_url method_ req in
    let rsp = observation_space_contains_response_of_string rsp in
    rsp.observation_space_contains_member
end

(** [env_monitor_start instance_id directory force resume] starts
    monitoring. [force] clears out existing training data from this
    directory (by deleting every file prefixed with [openaigym.]).
    [resume] retains the training data already in this directory,
    which will be merged with our new data.
*)
let env_monitor_start : instance_id -> string -> bool -> bool -> unit = begin
  fun instance_id directory force resume ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/monitor/start/" in
    let req =
      string_of_monitor_start_param
        { monitor_directory = directory;
          monitor_force = force;
          monitor_resume = resume;
          monitor_video_callable = false; }
    in
    let _rsp = Rest.post !base_url method_ req in
    assert (_rsp = "");
    ()
end

(** [env_monitor_close instance_id] flushes all monitor data to disk.
*)
let env_monitor_close : instance_id -> unit = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/monitor/close/" in
    let req = "" in
    let _rsp = Rest.post !base_url method_ req in
    assert (_rsp = "");
    ()
end

(** [env_close instance_id] stops the environment. *)
let env_close : instance_id -> unit = begin
  fun instance_id ->
    let method_ = "/v1/envs/"^instance_id.instance_id^"/close/" in
    let req = "" in
    let _rsp = Rest.post !base_url method_ req in
    assert (_rsp = "");
    ()
end

(** [shutdown_server ()] requests a server shutdown. *)
let shutdown_server : unit -> string = begin
  fun () ->
    let method_ = "/v1/shutdown/" in
    let req = "" in
    let rsp = Rest.post !base_url method_ req in
    rsp
end
