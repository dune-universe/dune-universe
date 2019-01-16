open Openai_gym
open Gym_j
open Gym_t

let instance_id =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_create@.";
  let instance_id = Gym_client.env_create "CartPole-v0" in
  Format.printf "%s@." instance_id.instance_id;
  instance_id

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_list_all@.";
  let envs = Gym_client.env_list_all () in
  List.iter
    (fun (instance_id, env_id) ->
       Format.printf "  %s: %s@." instance_id.instance_id env_id)
    envs

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_reset@.";
  let obs = Gym_client.env_reset instance_id in
  Format.printf "observation = %s@." (string_of_observation obs)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_step@.";
  let resp = Gym_client.env_step instance_id { action = 0; } false in
  Format.printf "resp = %s@." (string_of_step_response resp)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_action_space_info@.";
  let resp = Gym_client.env_action_space_info instance_id in
  Format.printf "resp = %s@." (string_of_action_space_info resp)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_action_space_sample@.";
  let resp = Gym_client.env_action_space_sample instance_id in
  Format.printf "action = %s@." (string_of_action resp)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_action_space_contains@.";
  let resp = Gym_client.env_action_space_contains instance_id 0 in
  Format.printf "member = %s@." (string_of_bool resp)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_observation_space_info@.";
  let resp = Gym_client.env_observation_space_info instance_id in
  Format.printf "obs_space = %s@." (string_of_observation_space_info resp)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_observation_space_contains@.";
  let resp =
    Gym_client.env_observation_space_contains instance_id (`Assoc[])
  in
  Format.printf "member = %s@." (string_of_bool resp)

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_monitor_start@.";
  let () =
    Gym_client.env_monitor_start instance_id "/tmp/gym-results" true false
  in
  Format.printf "monitor started@."

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_monitor_close@.";
  let () = Gym_client.env_monitor_close instance_id in
  Format.printf "monitor closed@."

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_close@.";
  let () = Gym_client.env_close instance_id in
  Format.printf "closed %s@." instance_id.instance_id

let () =
  Format.printf "-------------------------------@.";
  Format.printf "Test env_list_all@.";
  let envs = Gym_client.env_list_all () in
  List.iter
    (fun (instance_id, env_id) ->
       Format.printf "  %s: %s@." instance_id.instance_id env_id)
    envs

(* let () = *)
(*   Format.printf "-------------------------------@."; *)
(*   Format.printf "Test shutdown@."; *)
(*   let resp = Gym_client.shutdown_server () in *)
(*   Format.printf "server shutdowned: %s@." resp *)
