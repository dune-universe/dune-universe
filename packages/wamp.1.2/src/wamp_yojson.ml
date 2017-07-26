open Wamp

let remaining_args = function
  | [`List args] -> args, []
  | [`List args; `Assoc kwArgs] -> args, kwArgs
  | _ -> [], []

let msg_of_yojson = function
  | `List ((`Int typ) :: content) -> begin
      match msgtyp_of_enum typ with
      | None -> Result.Error Printf.(sprintf "msg_of_json: invalid msg type %d" typ)
      | Some HELLO -> begin
          match content with
          | [`String uri; `Assoc details] ->
            let realm = Uri.of_string uri in
            Ok (hello ~realm ~details)
          | _ -> Error "msg_of_yojson: HELLO"
        end
      | Some WELCOME -> begin
          match content with
          | [`Int id; `Assoc details] ->
            Ok (welcome ~id ~details)
          | _ -> Error "msg_of_yojson: WELCOME"
        end
      | Some ABORT -> begin
          match content with
          | [`Assoc details; `String reason] ->
            let reason = Uri.of_string reason in
            Ok (abort ~details ~reason)
          | _ -> Error "msg_of_yojson: ABORT"
        end
      | Some GOODBYE -> begin
          match content with
          | [`Assoc details; `String reason] ->
            let reason = Uri.of_string reason in
            Ok (goodbye ~details ~reason)
          | _ -> Error "msg_of_yojson: GOODBYE"
        end
      | Some ERROR -> begin
          match content with
          | `Int reqtype :: `Int reqid :: `Assoc details :: `String uri :: tl ->
            let uri = Uri.of_string uri in
            let args, kwArgs = remaining_args tl in
            Ok (error ~reqtype ~reqid ~details ~error:uri ~args ~kwArgs)
          | _ -> Error "msg_of_yojson: ERROR"
        end
      | Some PUBLISH -> begin
          match content with
          | `Int reqid :: `Assoc options :: `String topic :: tl ->
            let topic = Uri.of_string topic in
            let args, kwArgs = remaining_args tl in
            Ok (publish ~reqid ~options ~topic ~args ~kwArgs)
          | _ -> Error "msg_of_yojson: PUBLISH"
        end
      | Some PUBLISHED -> begin
          match content with
          | [`Int reqid; `Int id] ->
              Ok (published ~reqid ~id)
          | _ -> Error "msg_of_yojson: PUBLISHED"
        end
      | Some SUBSCRIBE -> begin
          match content with
          | [`Int reqid; `Assoc options; `String topic] ->
            let topic = Uri.of_string topic in
            Ok (subscribe reqid options topic)
          | _ -> Error "msg_of_yojson: PUBLISH"
        end
      | Some SUBSCRIBED -> begin
          match content with
          | [`Int reqid; `Int id] ->
              Ok (subscribed ~reqid ~id)
          | _ -> Error "msg_of_yojson: SUBSCRIBED"
        end
      | Some UNSUBSCRIBE -> begin
          match content with
          | [`Int reqid; `Int id] ->
              Ok (unsubscribe ~reqid ~id)
          | _ -> Error "msg_of_yojson: UNSUBSCRIBE"
        end
      | Some UNSUBSCRIBED -> begin
          match content with
          | [`Int reqid] -> Ok (unsubscribed reqid)
          | _ -> Error "msg_of_yojson: UNSUBSCRIBED"
        end
      | Some EVENT -> begin
          match content with
          | `Int subid :: `Int pubid :: `Assoc details :: tl ->
            let args, kwArgs = remaining_args tl in
            Ok (event ~subid ~pubid ~details ~args ~kwArgs)
          | _ -> Error "msg_of_yojson: EVENT"
        end
    end
  | #Yojson.Safe.json as json -> Error Yojson.Safe.(to_string json)

let msg_to_yojson = function
  | Hello { realm; details } ->
    `List [`Int (msgtyp_to_enum HELLO); `String (Uri.to_string realm); `Assoc details]
  | Welcome { id; details } ->
    `List [`Int (msgtyp_to_enum WELCOME); `Int id; `Assoc details ]
  | Abort { details; reason } ->
    `List [`Int (msgtyp_to_enum ABORT); `Assoc details; `String (Uri.to_string reason) ]
  | Goodbye { details; reason } ->
    `List [`Int (msgtyp_to_enum GOODBYE); `Assoc details; `String (Uri.to_string reason) ]
  | Error { reqtype; reqid; details; error; args; kwArgs } ->
    `List [`Int (msgtyp_to_enum ERROR); `Int reqtype; `Int reqid; `Assoc details; `String (Uri.to_string error); `List args; `Assoc kwArgs]
  | Publish { reqid; options; topic; args; kwArgs } ->
    `List [`Int (msgtyp_to_enum PUBLISH); `Int reqid; `Assoc options; `String (Uri.to_string topic); `List args; `Assoc kwArgs]
  | Published { reqid; id } ->
    `List [`Int (msgtyp_to_enum PUBLISHED); `Int reqid; `Int id]
  | Subscribe { reqid; options; topic } ->
    `List [`Int (msgtyp_to_enum SUBSCRIBE); `Int reqid; `Assoc options; `String (Uri.to_string topic)]
  | Subscribed { reqid; id } ->
    `List [`Int (msgtyp_to_enum SUBSCRIBED); `Int reqid; `Int id]
  | Unsubscribe { reqid; id } ->
    `List [`Int (msgtyp_to_enum UNSUBSCRIBE); `Int reqid; `Int id]
  | Unsubscribed reqid ->
    `List [`Int (msgtyp_to_enum UNSUBSCRIBED); `Int reqid]
  | Event { subid; pubid; details; args; kwArgs } ->
    `List [`Int (msgtyp_to_enum EVENT); `Int subid; `Int pubid; `Assoc details; `List args; `Assoc kwArgs]

let hello realm roles =
  let roles = ListLabels.map roles ~f:(fun r -> string_of_role r, `Assoc []) in
  hello ~realm ~details:["roles", `Assoc roles]

let subscribe ?(reqid=Random.bits ()) ?(options=[]) topic =
  reqid, (subscribe reqid options topic)
