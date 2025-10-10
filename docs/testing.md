# Testing Documentation

There are a few different testing frameworks used for hasell, including
QuickCheck, HUnit, and Hspec among others.

[QuickCheck](https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
is more akin to requirements testing, i.e. one specifies properties
the program (or part of it) should fulfil which are used to generate test cases.
In case you took the software reliability course (DD2459) this is somewhat similar to JML.

[HUnit](https://hackage.haskell.org/package/HUnit) as the name suggests,
is a unit testing framework similar to JUnit.
This is a black-box testing approach, where the program (or part of it) is
run for some input or scenario and the output is tested against some correctness criteria.
The implementation details are not important other than the public interfaces.

[Hspec](https://hspec.github.io/) integrates several testing framework into a common interface,
such as HUnit and QuickCheck. This is what we will mainly use in this project.

Additionally, GHC has some built-in tools for coverage testing (a glass-box testing technique)
in the form of [HPC](https://wiki.haskell.org/Haskell_program_coverage).

Sample run:
```
$ ghc -fhpc -package ghc src/Main.hs
$ ./src/Main
...
$ hpc report Main
 95% expressions used (38/40)
100% boolean coverage (0/0)
     100% guards (0/0)
     100% 'if' conditions (0/0)
     100% qualifiers (0/0)
100% alternatives used (0/0)
100% local declarations used (0/0)
100% top-level declarations used (1/1)
```
Or with cabal: `cabal run --enable-coverage` (though this seems to generate the .mix and .tix
files in unexpected locations and can't be directly used by hpc.

The tool can also generate HTML output with `hpc markup`, which among other things
can annotate the source and show which parts of it have been exercised or not.
