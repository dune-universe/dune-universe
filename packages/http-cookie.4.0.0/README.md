# Cookies

A comprehensive and standards compliant HTTP cookies library for ocaml.

HTTP cookies are serialized as follows:

- In a `Cookie` header in a HTTP request
- In a `Set-Cookie` header in a HTTP response.

The library supports consuming and creating HTTP cookies in both requests
and responses.

The standards implemented by the library is 
- [Cookies - RFC 6265](https://tools.ietf.org/html/rfc6265)
- [HTTP Date - RFC 1123](https://datatracker.ietf.org/doc/html/rfc1123) 
- [Domain Name - RFC 1034](https://datatracker.ietf.org/doc/html/rfc1034#section-3.5)
- [Hosts - RFC 1123](https://datatracker.ietf.org/doc/html/rfc1123#section-2.1)
- [IPv4/IPv6](https://datatracker.ietf.org/doc/html/draft-main-ipaddr-text-rep-02#section-3})

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
