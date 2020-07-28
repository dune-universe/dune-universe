let break_time_s = 5 * 60
let work_time_s = 25 * 60

let clock_color = ANSITerminal.on_green::[]
let clock_background = ANSITerminal.on_default::[]

let icon_size_x = 5
let icon_size_y = 5

let digit_spacing = 1
let digit_width = icon_size_x + digit_spacing
let total_digits = 5
let clock_width = (digit_width * total_digits)
let half_clock_width = clock_width / 2
let clock_height = icon_size_y
let half_clock_height = clock_height / 2
