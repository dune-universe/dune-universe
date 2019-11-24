type time = int * int * int

type user = string

type msg = string

type notice = string

type action = string

type content = Msg of user * msg | Notice of notice | Action of action

type line = time * content
