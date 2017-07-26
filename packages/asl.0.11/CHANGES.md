## 0.11 (2017-05-22)
- Use topkg
- Use jbuilder

## 0.10 (2016-05-22)
- Fix bug where an exception is thrown at module load time on non-OSX
  platforms

## 0.9 (2016-05-19)
- Will now build on non-OSX platforms, although a Failure exception
  will be thrown at runtime if anyone tries to use it.

## 0.8 (2016-04-07)
- Add an explicit LICENSE file

## 0.7 (2016-04-06)
- Add a reporter, for use with the `logs` package
- Add `Client.add_output_file` for logging to external files
