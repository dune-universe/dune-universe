# Time

Daypack-lib provides some level of time handling functionality, but they were primarily designed
for internal use, thus may not be as general as a "time library" would be

Daypack-lib components primarily deal with time in seconds since Unix epoch in `int64`,
but a date time representation is available, which the time zone information is specified
by the offset in seconds (following `Ptime`).
