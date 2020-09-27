# ocaml-dkim

`ocaml-dkim` is a pure implementation of DKIM in OCaml. It permits to verify and
sign an incoming email. It can be use as a SMTP filter service (verify) or as a
SMTP submission service (sign).

### How to install it?

You must have an OPAM environment. Then, `ocaml-dkim` can be installed with:

```sh
$ opam pin add https://github.com/dinosaure/ocaml-dkim.git
```

### How to use it?

`ocaml-dkim` provides 2 binaries, one to verify, the second to sign an email.

```sh
$ dkim.verify test/raw/001.mail
[ok]: sendgrid.info
[ok]: github.com
```

It shows all domains which signed the given email and whether the signature is
correct or not (for the last case, it shows you the _selector_). `ocaml-dkim` is
able to sign an email from a private RSA key and a specific domain such as:

```sh
$ dkim.sign -k private-key.pem --selector admin --hostname x25519.net test/raw/001.mail
DKIM-Signature: ...
Rest of the email
```

It prints the signed email then. The user is able to use a specific RSA private
key or it can use a seed used to generate the RSA private key with the _fortuna_
random number generator.
