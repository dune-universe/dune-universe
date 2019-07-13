# Mirage-Logs

A reporter for the [Logs][] library that writes log messages to `stderr`, using
a Mirage `CLOCK` to add timestamps.

It can also log only important messages to the console, while writing all received messages to a ring buffer which is displayed if an exception occurs.

If [Mirage tracing][] is enabled, it also writes each log message to the trace buffer.

See `mirage_logs.mli` for details.

[Logs]: http://erratique.ch/software/logs
[Mirage tracing]: https://github.com/mirage/mirage-profile

