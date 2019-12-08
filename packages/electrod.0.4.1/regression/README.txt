# To CREATE a test for a file `foo.elo`:
1. copy `foo.elo` here
2. add a `foo.md` file with EXACT content (including backquotes, tabulations...):

```sh
  $ ../src/electrod.exe foo.elo
```

3. update `dune` file with:
3a. update `deps` section by creating a new variable holding name `foo.md`
3b. update the `action` section with two lines for running `mdx` and checking if we have a diff


# To RUN tests FROM PROJECT BASE DIRECTORY
Issue the following command:
$ make regression


If a test finishes with a test error (i.e. the diff is non-empty) BUT this is correct, you want to record that this is the expected output. To record the update, issue the following command:
$ dune promote regression/foo.md

Else, if the test error was unexpected: investigate manually!
