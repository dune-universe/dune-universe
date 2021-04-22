# Prettym, a bounded encoder with the colums constraint

`prettym` is a simple _bounded_ encoder to serialize human readable values
and respect the 80-column constraint. It permits to serialize values in
the respect of RFC 822 and put `fws` token when necessary.

For example, a list of email addresses should fits under the 80 column for
an email. The encoder should find the opportunity to add a line breaker plus
a space to respect RFC 822.
