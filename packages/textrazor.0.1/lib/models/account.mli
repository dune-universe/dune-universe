(** Data structure for TextRazor accounts. *)
type t = {
  concurrent_request_limit: int;
  concurrent_requests_used: int;
  plan_daily_included_requests: int;
  plan: string;
  requests_used_today: int;
} [@@deriving of_yojson {strict = false}]
