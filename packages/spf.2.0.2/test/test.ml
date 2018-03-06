open Printf

let check_from server client_addr from =
  try
    let r = SPF.check_from server client_addr from in
    `Response r
  with SPF.Error e ->
    `Error e

let check_helo server client_addr helo =
  try
    let r = SPF.check_helo server client_addr helo in
    `Response r
  with SPF.Error e ->
    `Error e

let () =
  let server = SPF.server SPF.Dns_cache in
  let client_addr = Unix.inet_addr_of_string "187.73.32.159" in
  let _helo = "mta98.f1.k8.com.br" in
  let from = "andre@andrenathan.com" in
  match check_from server client_addr from with
  | `Error e ->
      printf "SPF error: %s\n%!" e
  | `Response r ->
      printf "SPF response:\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n%!"
        (SPF.string_of_result (SPF.result r))
        (SPF.string_of_reason (SPF.reason r))
        (SPF.received_spf r)
        (SPF.received_spf_value r)
        (SPF.header_comment r);
      match SPF.result r with
      | SPF.Neutral c
      | SPF.Fail c
      | SPF.Softfail c ->
          printf "\t\t%s\n\t\t%s\n%!"
            (SPF.explanation c)
            (SPF.smtp_comment c)
      | _ -> ()

let () =
  let server = SPF.server ~debug:false SPF.Dns_cache in
  let client_addr = Unix.inet_addr_of_string "189.57.226.93" in
  let helo = "gwmail.bradescoseguros.com.br" in
  let _from = "andre@bradescoseguros.com.br" in
  match check_helo server client_addr helo with
  | `Error e ->
      printf "SPF error: %s\n%!" e
  | `Response r ->
      printf "SPF response:\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n%!"
        (SPF.string_of_result (SPF.result r))
        (SPF.string_of_reason (SPF.reason r))
        (SPF.received_spf r)
        (SPF.received_spf_value r)
        (SPF.header_comment r);
      printf ">>>\n%!";
      match SPF.result r with
      | SPF.Neutral c
      | SPF.Fail c
      | SPF.Softfail c ->
          printf "\t\t%s\n\t\t%s\n%!"
            (SPF.explanation c)
            (SPF.smtp_comment c)
      | _ -> ()
