type t = { user : string; repo : string; branch : string }

let pp ppf { user; repo; branch } =
  Format.fprintf ppf "{ user = %S; repo = %S; branch = %S }" user repo branch

let git_url { user; repo; branch } =
  Format.asprintf "git+https://github.com/%s/%s#%s" user repo branch
