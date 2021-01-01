`reddit_api` is a set OCaml client libraries for Reddit's API.

`reddit_api_kernel` provides:

* Types for representing Reddit's API parameters and responses
* Functions for building HTTP requests and handling the associated responses
  for Reddit's API endpoints.

`reddit_api_async` provides a client for sending these requests to Reddit and
some utilities for common usage patterns. It handles authentication and
Reddit's rate-limiting headers.

# Documentation

[Here](https://leviroth.github.io/ocaml-reddit-api/). I recommend the
[`Reddit_api_kernel.Api`](https://leviroth.github.io/ocaml-reddit-api/reddit_api_kernel/Reddit_api_kernel/Api/index.html)
and
[`Reddit_api_async.Connection`](https://leviroth.github.io/ocaml-reddit-api/reddit_api_async/Reddit_api_async/Connection/index.html)
modules as entry points.

# Example

```ocaml
let print_links credentials =
  let connection = Connection.create credentials ~user_agent:"Link printer" in
  let subreddit =
    Subreddit_name.combine (List.map ~f:Subreddit_name.of_string [ "ocaml"; "redditdev" ])
  in
  let%bind link_listing =
    Connection.call_exn connection (Api.top ~since:Year ~subreddit ())
  in
  let links = Listing.children link_listing in
  List.iter links ~f:(fun link ->
      print_s
        [%sexp
          { title : string = Thing.Link.title link
          ; url : string option = Option.map (Thing.Link.url link) ~f:Uri.to_string
          ; author : Username.t option = Thing.Link.author link
          ; score : int = Thing.Link.score link
          }]);
  return ()
```

# Goals and non-goals

## Goals

- Provide a typed interface to Reddit's API endpoints and responses.
- Encode knowledge, documented or otherwise, about correct usage of the API via
  the type system.
  - Don't raise exceptions if Reddit is behaving "as expected."
  - Express corner cases in response types. For example, are there surprising
    cases where a field might not be present? Make the field optional instead
    of making each user discover this on their own.
  - Handle common Reddit server issues such as 503 errors automatically, or
    else warn about them via the response type.
- Provide workarounds when we get the above wrong:
  - A `?param_list_override:((string * string list) list -> (string * string
    list) list)` option on each API endpoint that allows the HTTP parameters to
    be manipulated directly.
  - A `Connection.call_raw` function that allows users to access HTTP responses
    directly.

## Non-goals

- Be perfect
  - Reddit's API is not very well documented.  Determining which inputs and
    outputs are legal is largely a matter of trial and error. At any given
    time, it's likely that we allow some invalid combination of inputs, or
    forbid a valid combination, or fail to handle some valid response.
- Express "unexpected" Reddit behavior in the type system.
  - If we get something from Reddit that we don't understand, we'll just raise.
    We don't make every function return a ``(_, [`couldn't_parse_response])
    Result.t)``.

# Credits

Thanks to [PRAW](https://github.com/praw-dev/praw/) for providing innumerable
examples of Reddit API client code.
