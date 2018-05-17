This is a deprecated version of partition maps that uses an association lists
for the large data structure and bit-vectors to keep track which domain
elements are associated with a value.

The use of an association list, in general, has not changed. But using intervals
has forced a different, non continuation-passing style of writing some of
merging (here called map). It is debatable how much overhead, or improvment,
was caused by this style. It is retained here for comparison and reflection.
