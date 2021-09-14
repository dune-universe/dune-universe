let seqs = {eof|>s1 apple
ACTG
n
>s2 pie
actg
n
|eof}

let all_carets = {eof|>
>>
>>>
>
|eof}

let tricky_seqs =
  {eof|> empty seq at beginning
>seq1 is fun
AAC TGG NN N


>seq2
     AAT
CCTGNNN
> empty seq 1
> empty seq 2


>seq3
yyyyyyyyyy

     yyyyy
NNN
>seq 4 > has many '>' in header
ACTG
actg
>seq 5
          a      c     t     G
>empty seq at end|eof}

let bad_file = "\r"

let weird_blank_lines = {eof|

>s1

A

|eof}

let empty_header_lines = {eof|>
ACTG
>
actg
|eof}
