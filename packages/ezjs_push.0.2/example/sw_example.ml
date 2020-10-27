open Ezjs_min
open Ezjs_push

let () =
  onactivate (fun _ev ->
      log_str "service worker activation";
      let options = {
        so_user_visible_only = Some true;
        so_application_server_key = Some "BMDc6lvxIgN4aluWtZSZokp5GcJedJwgHUzRUVqFlTHzJl26o4eZTdokJ7svKn7MyaugUXeOc00F7cPh_922-Os"
      } in
      subscription ~options self##.registration (fun subscr ->
          js_log subscr;
          js_log @@ subscr##toJSON);
      );
  onpush (fun e ->
      log_str "received a push message";
      let data = Opt.case e##.data (fun () -> "no payload") (fun d -> to_string d##text) in
      let options = {empty_notif with no_body = Some data} in
      show_notification ~options self##.registration "test")
