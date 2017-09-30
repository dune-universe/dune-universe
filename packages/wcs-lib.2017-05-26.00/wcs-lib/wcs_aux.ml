type selector =
  | Goto_user_input
  | Goto_condition
  | Goto_body

let selector_wrap s =
  begin match s with
  | "user_input" | "client" -> Goto_user_input
  | "condition" -> Goto_condition
  | "body" -> Goto_body
  | _ -> Log.error "Wcs_aux" (Some Goto_user_input) ("bad selector: "^s)
  end

let selector_unwrap s =
  begin match s with
  | Goto_user_input -> "user_input"
  | Goto_condition -> "condition"
  | Goto_body -> "body"
  end
