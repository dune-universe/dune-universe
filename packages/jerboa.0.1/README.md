# Jerboa: minimalistic web framework for everyone
Jerboa is web framework, which is:
- Minimalistic: It will give you the building blocks, but nothing more
- Flexible: It let's you be as flexible as you want to be
- Easy to use: You only need to understand the simple building blocks to use it

## Documentation
[API Documentation](https://strykerkkd.github.io/Jerboa/)

## How to use it?
This how to is based on the example that you can find in the examples directory.

First install Jerboa via opam: `opam install jerboa`

 The entry point of the framework is the `Jerboa.start` function, which accepts:
- `port` (optional, default is 8080)
- `default_request_handler` (optional, default gives back http 404 error)
- `middleware_config` (optional, default is empty middleware config)
- `path_handler_config` (required)

### Path handler config
From these arguments the most important is the path handler config, which is basically a list of path handlers.
A path handler's job is to handle the incoming request's that match it's criteria. A path handler has three components:
- `meth`: the http method to match
- `path_mapping`: request's path\route to match
- `request_handler`: a function, which will handle the incomming request by turning it into a response

You can create path handlers easily with the `Jerboa.Path_Handler.create` function, which arguments are the ones mentioned above.

For example the following creates a path handler for request where:
- `meth` is a GET request and
- `path_mapping` matches the `/hello/<something>` path:
```ocaml
let my_path_handler = 
  let open Jerboa in
  Path_handler.create `GET [Path.const "hello"; Path.var "name"] (fun request ->
      let open Request in
      let found_path_parameter = Base.List.Assoc.find request.path_parameter ~equal:(=) "name" in
      Response.create 200 ("Hello " ^ (Base.Option.value found_path_parameter ~default:"not found")) 
    )
```
If we get the specified reuquest than Jerboa will call our `request_handler`, which will reponde with the `"Hello <something>"` message.

#### Path mapping
In the above example we could see that the `path_mapping` is `[Path.const "hello"; Path.var "name"]`, which means that:
- `Path.const "hello"` means that the first part of the route must be equal to `"hello"`, but it won't be captured as a variable, because it's a constatnt value
- `Path.var "name"` means that the second part of the route can be anything, but we are capturing it's value with the `name` path parameter

Path mapping in Jerboa is pretty flexible, because you can make your own regex based path matchings with:
- `Path.create_const regex`, which create a constant path part with the supplied regex
- `Path.create_var name regex`, which creates a path variable with the supplied variable name and regex

#### Request handler
Request handler is basically a function that transforms request(`Request.t`) into a response(`Response.t`). In the above example the following was the `request_handler`:
```ocaml
(fun request ->
  let open Request in
  let found_path_parameter = Base.List.Assoc.find request.path_parameter ~equal:(=) "name" in
  Response.create 200 ("Hello " ^ (Base.Option.value found_path_parameter ~default:"not found")) 
)
```
The most important function used in the `request_handler` is the `Response.create` function, which inputs are the http response method and the body of the response.

### Middleware config
Middleware config is a list of middlewares and a middleware is a function, which updates the content of the request(`Request.t`) by creating a new request based on the old one. The following example show this in action:
```ocaml
let my_middleware request = 
  let open Jerboa.Request in
  if request.path = "/" then 
    {request with path = "/hello/world"}
  else
    request
```
The above example updates the request's path to point to `/hello/world`, when a request comes with the `/` path.

### Default request handler
The default request handler comes into play, when Jerboa can't find a matching `path_handler` for the incoming request. The default `default_request_handler` gives back an empty http 404 response.