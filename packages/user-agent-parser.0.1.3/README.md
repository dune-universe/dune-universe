# user-agent-parser (ocaml)

[![Build Status](https://travis-ci.org/issuu/uap-ocaml.svg?branch=master)](https://travis-ci.org/issuu/uap-ocaml)

Ocaml implementation of the user agent parse rules from https://github.com/ua-parser/uap-core

Documentation: https://issuu.github.io/uap-ocaml/

## Example

```ocaml
(* init compiles a bunch of regexes, so try to do this just once *)
let uaparser = User_agent_parser.init () in

let user_agent = "Mozilla/5.0 (Windows; Windows NT 5.1; rv:2.0b3pre) Gecko/20100727 Minefield/4.0.1pre" in
let parsed = User_agent_parser.parse uaparser user_agent in
Printf.printf "%s\n" parsed.ua.family;                     (* Firefox (Minefield) *)
Printf.printf "%s %s\n" parsed.os.family parsed.os.major;  (* Windows XP *)
Printf.printf "%s\n" parsed.device.family;                 (* Other *)

let user_agent = "Mozilla/5.0 (Linux; U; Android 4.2.2; de-de; PEDI_PLUS_W Build/JDQ39) AppleWebKit/534.30 (KHTML, like Gecko) Version/4.0 Safari/534.30" in
let parsed = User_agent_parser.parse uaparser user_agent in
Printf.printf "%s\n" parsed.ua.family;                     (* Android *)
Printf.printf "%s %s\n" parsed.os.family parsed.os.major;  (* Android 4 *)
Printf.printf "%s\n" parsed.device.family                  (* Odys PEDI PLUS W *)
```

## License

Apache 2.0, see [LICENSE](LICENSE).

