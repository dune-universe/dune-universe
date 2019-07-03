# ParDi

Parallel and Distributed execution of command lines, pardi !

# Example

Compress a file in parallel using 1MB chunks:
```
pardi -d b:1048576 -m s -i <YOUR_BIG_FILE> -o <YOUR_BIG_FILE>.gz -w 'xz -c -9 %IN > %OUT'

```
