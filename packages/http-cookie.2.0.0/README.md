# Cookies

A comprehensive and standards compliant HTTP cookies library for ocaml.

HTTP cookies are serialized as follows:

- In a `Cookie` header in a HTTP request
- In a `Set-Cookie` header in a HTTP response.

The library supports consuming and creating HTTP cookies in both requests
and responses.

The standard implemented by the library is [RFC 6265](https://tools.ietf.org/html/rfc6265).

[API Documentation](https://lemaetech.co.uk/http-cookie/)

## Installation

```sh
$ opam install http-cookie
```

## Usage

Create a cookie,

```ocaml
Http_cookie.create "SID" "23432324"
```

Serialize cookie to a HTTP `Set-Cookie` header value,

```ocaml
let s = Http_cookie.to_set_cookie_header_value c in
s = "SID=31d4d96e407aad42; Path=/; Secure; HttpOnly; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
```
