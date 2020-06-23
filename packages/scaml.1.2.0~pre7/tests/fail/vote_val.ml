open SCaml
open App_vote

let initial_storage = {
  config = { title = "Do you like curry?"                      (* 投票のタイトルを設定 *)
           ; beginning_time = Timestamp "2020-01-01T00:00:00Z" (* 投票の開始時刻を設定 *)
           ; finish_time = Timestamp "2020-12-31T23:59:59Z"    (* 投票の終了時刻を設定 *)
           }
  ; candidates = Map [ ("Yes", Int 0); ("No", Int 0) ] (* 票数は0で初期化 *)
  ; voters = Set []                                    (* 初期状態で投票済みのアドレスはない *)
  }
