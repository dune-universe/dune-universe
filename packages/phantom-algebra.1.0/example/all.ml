[@@@warning "-33"]
open Readme
open Indexing
open A

;; if !Tools.failure then
  Format.printf "\x1b[91m⛅⛅⛅⛅⛅⛅⛅⛅⛅FAILURE⛅⛅⛅⛅⛅⛅⛅⛅⛅\x1b[97m@."
else Format.printf
    "\x1b[33m☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼\x1b[92mSuccess\x1b[33m☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼☼\x1b[97m@."
