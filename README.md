# secret-santa
Generates secret santa pairings, and anonymously emails people their matches!

## Building
`secret-santa` is most easily built using [Stack](http://haskellstack.org). This can be this installed using most package managers (the package is usually called `haskell-stack`), or downloaded from the Stack [website](http://docs.haskellstack.org/en/stable/README.html#how-to-install).

Once you have `Stack`, do the following to build `secret-santa`:

1. Clone this repository: Navigate to where you want the files to be, and type `git clone git@github.com:devonhollowood/secret-santa.git`.
1. Change directories to `secret-santa`: Type `cd secret-santa`.
1. Build `secret-santa` using `Stack`: Type `stack build`. It may take a minute or two to download and build the dependencies. You may be prompted to run `stack setup` first. If this occurs, do so and then re-run `stack build`.
1. You have now built `secret-santa`! `Stack` will tell you where it put the executable. You can either use that, or use `stack exec` as documented below.

You will also need a gmail account if you would like to have to program email matches.

## Running
`secret-santa` has two modes:

* The `test` mode will assign secret santas, but will just print them to the command line. This is useful for making sure that you have everything configured correctly. These assignments are not saved anywhere, and are fresh each time.
* The `execute` mode will assign secret santas, and email them through your gmail account. It will prompt you for your gmail username (e.g. `devonhollowood`) and password before emailing. It will then create a file, `already-sent.guard`, the existance of which is checked before sending future emails. This helps prevent accidental double-sends. In order to send again, delete this file.

To run the program, type `stack exec -- secret-santa <mode> <people file>`, where `<mode>` is either `test` or `execute` (documented above), and `<people file>` is a csv file containing a list of people and their email addresses.

You may also supply an optional argument, `--forbidden-pairs <forbidden pairs file>`, where `<forbidden pairs file> is a csv file containing a list of people who may not be paired (perhaps they are best friends or dating, and were planning on getting each other gifts anyways). This argument may be abbreviated `-x <forbidden pairs file>`.

Examples of `<people file>` and `<forbidden pairs file>` maybe be found in the `examples/` directory.

## Enjoy, and get each other good stuff!
