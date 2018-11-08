let my_middleware request = 
  let open Jerboa.Request in
  if request.path = "/" then 
    {request with path = "/hello/world"}
  else
    request

let my_middleware_config = [my_middleware]

let my_path_handler = 
  let open Jerboa in
  Path_handler.create `GET [Path.const "hello"; Path.var "name"] (fun request ->
      let open Request in
      let found_path_parameter = Base.List.Assoc.find request.path_parameter ~equal:(=) "name" in
      Response.create 200 ("Hello " ^ (Base.Option.value found_path_parameter ~default:"not found")) 
    )

let my_path_handler_config = [my_path_handler]

let _ = Jerboa.start ~middleware_config:my_middleware_config my_path_handler_config