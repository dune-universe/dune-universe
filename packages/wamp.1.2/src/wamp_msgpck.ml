module M = Msgpck
open Wamp

let dict_of_map m =  ListLabels.map m ~f:(function M.String k, v -> k, v | _ -> invalid_arg "dict_of_map")
let map_of_dict d = ListLabels.map d ~f:(fun (k, v) -> M.String k, v)

let remaining_args = function
| [M.List args] -> args, []
| [M.List args; Map kwArgs] -> args, dict_of_map kwArgs
| _ -> [], []

let msg_of_msgpck = function
| M.List (Int typ :: content) -> begin match msgtyp_of_enum typ with
  | None -> Result.Error Printf.(sprintf "msg_of_json: invalid msg type %d" typ)
  | Some HELLO -> begin match content with
    | [String uri; Map details] ->
        let realm = Uri.of_string uri in
        let details = dict_of_map details in
        Ok (hello ~realm ~details)
    | _ -> Error "msg_of_msgpck: HELLO"
    end
  | Some WELCOME -> begin match content with
    | [Int id; Map details] ->
        let details = dict_of_map details in
        Ok (welcome ~id ~details)
    | _ -> Error "msg_of_msgpck: WELCOME"
    end
  | Some ABORT -> begin match content with
    | [Map details; String reason] ->
        let reason = Uri.of_string reason in
        let details = dict_of_map details in
        Ok (abort ~details ~reason)
    | _ -> Error "msg_of_msgpck: ABORT"
    end
  | Some GOODBYE -> begin
      match content with
      | [Map details; String reason] ->
          let reason = Uri.of_string reason in
          let details = dict_of_map details in
          Ok (goodbye ~details ~reason)
      | _ -> Error "msg_of_msgpck: GOODBYE"
    end
  | Some ERROR -> begin
      match content with
      | Int reqtype :: Int reqid :: Map details :: String uri :: tl ->
          let uri = Uri.of_string uri in
          let details = dict_of_map details in
          let args, kwArgs = remaining_args tl in
          Ok (error ~reqtype ~reqid ~details ~error:uri ~args ~kwArgs)
      | _ -> Error "msg_of_msgpck: ERROR"
    end
  | Some PUBLISH -> begin
      match content with
      | Int reqid :: Map options :: String topic :: tl ->
          let topic = Uri.of_string topic in
          let options = dict_of_map options in
          let args, kwArgs = remaining_args tl in
          Ok (publish ~reqid ~options ~topic ~args ~kwArgs)
      | _ -> Error "msg_of_msgpck: PUBLISH"
    end
  | Some PUBLISHED -> begin
      match content with
      | [Int reqid; Int id] ->
          Ok (published ~reqid ~id)
      | _ -> Error "msg_of_msgpck: PUBLISHED"
    end
  | Some SUBSCRIBE -> begin
      match content with
      | [Int reqid; Map options; String topic] ->
          let topic = Uri.of_string topic in
          let options = dict_of_map options in
          Ok (subscribe reqid options topic)
      | _ -> Error "msg_of_msgpck: PUBLISH"
    end
  | Some SUBSCRIBED -> begin
      match content with
      | [Int reqid; Int id] ->
          Ok (subscribed ~reqid ~id)
      | _ -> Error "msg_of_msgpck: SUBSCRIBED"
    end
  | Some UNSUBSCRIBE -> begin
      match content with
      | [Int reqid; Int id] ->
          Ok (unsubscribe ~reqid ~id)
      | _ -> Error "msg_of_msgpck: UNSUBSCRIBE"
    end
  | Some UNSUBSCRIBED -> begin
      match content with
      | [Int reqid] -> Ok (unsubscribed reqid)
      | _ -> Error "msg_of_msgpck: UNSUBSCRIBED"
    end
  | Some EVENT -> begin
      match content with
      | Int subid :: Int pubid :: Map details :: tl ->
          let details = dict_of_map details in
          let args, kwArgs = remaining_args tl in
          Ok (event ~subid ~pubid ~details ~args ~kwArgs)
      | _ -> Error "msg_of_msgpck: EVENT"
    end
  end
| msg -> Error "msg_of_msgpck: msg must be a List"

let msg_to_msgpck = function
| Hello { realm; details } ->
    let details = map_of_dict details in
    M.List [Int (msgtyp_to_enum HELLO); String (Uri.to_string realm); Map details]
| Welcome { id; details } ->
    let details = map_of_dict details in
    List [Int (msgtyp_to_enum WELCOME); Int id; Map details ]
| Abort { details; reason } ->
    let details = map_of_dict details in
    List [Int (msgtyp_to_enum ABORT); Map details; String (Uri.to_string reason) ]
| Goodbye { details; reason } ->
    let details = map_of_dict details in
    List [Int (msgtyp_to_enum GOODBYE); Map details; String (Uri.to_string reason) ]
| Error { reqtype; reqid; details; error; args; kwArgs } ->
    let details = map_of_dict details in
    let kwArgs = map_of_dict kwArgs in
    List [Int (msgtyp_to_enum ERROR); Int reqtype; Int reqid; Map details; String (Uri.to_string error); List args; Map kwArgs]
| Publish { reqid; options; topic; args; kwArgs } ->
    let options = map_of_dict options in
    let kwArgs = map_of_dict kwArgs in
    List [Int (msgtyp_to_enum PUBLISH); Int reqid; Map options; String (Uri.to_string topic); List args; Map kwArgs]
| Published { reqid; id } ->
    List [Int (msgtyp_to_enum PUBLISHED); Int reqid; Int id]
| Subscribe { reqid; options; topic } ->
    let options = map_of_dict options in
    List [Int (msgtyp_to_enum SUBSCRIBE); Int reqid; Map options; String (Uri.to_string topic)]
| Subscribed { reqid; id } ->
    List [Int (msgtyp_to_enum SUBSCRIBED); Int reqid; Int id]
| Unsubscribe { reqid; id } ->
    List [Int (msgtyp_to_enum UNSUBSCRIBE); Int reqid; Int id]
| Unsubscribed reqid ->
    List [Int (msgtyp_to_enum UNSUBSCRIBED); Int reqid]
| Event { subid; pubid; details; args; kwArgs } ->
    let details = map_of_dict details in
    let kwArgs = map_of_dict kwArgs in
    List [Int (msgtyp_to_enum EVENT); Int subid; Int pubid; Map details; List args; Map kwArgs]

let hello realm roles =
  let roles = ListLabels.map roles ~f:M.(fun r -> String (string_of_role r), Map []) in
  hello ~realm ~details:["roles", M.Map roles]

let subscribe ?(reqid=Random.bits ()) ?(options=[]) topic =
  reqid, (subscribe reqid options topic)
