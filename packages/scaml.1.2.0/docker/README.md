# Build

```
./build.sh <commit>
```

should build an image `dailambda/scaml:<commit>`

# Basic use

`scamlc` script in this directory provides an easy way to launch the compiler in the docker image:

```
$ ./scamlc <args>
```

For example,

```
$ ls xxx.ml
xxx.ml
$ ./scamlc xxx.ml
```
