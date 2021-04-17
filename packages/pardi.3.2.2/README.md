# ParDi

Parallel and Distributed execution of command lines, pardi !

# Example

Compress a file in parallel using 1MB chunks:
```
pardi -d b:1048576 -p -i <YOUR_BIG_FILE> -o <YOUR_BIG_FILE>.xz -w 'xz -c -9 %IN > %OUT'
```

Standardize molecules in parallel:
```
pip3 install chemo-standardizer
pardi -i input.smi -o output_std.smi -c 400 -d l -ie '.smi' -oe '.smi' \
      -w 'standardiser -i %IN -o %OUT 2>/dev/null'
```

# Install

For beginners or non opam users: download and execute the latest self-installer
shell script from (https://github.com/UnixJunkie/pardi/releases).

Then, execute:
```
./pardi-2.0.1.sh ~/usr/pardi-2.0.1
```

This will create ~/usr/pardi-2.0.1/bin/pardi.

For opam users:
```
opam install pardi
```

Do not hesitate to contact the author in case of troubles or if
you have any question.
