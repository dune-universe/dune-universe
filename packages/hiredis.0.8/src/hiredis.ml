module Value = Hiredis_value
include Hiredis_client
include Hiredis_shell

let command arr =
    C.redis_format_command arr

let command_v arr =
    command (Array.map Hiredis_value.to_string arr)


