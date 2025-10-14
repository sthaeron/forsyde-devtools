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


## Usage in this project

There is a sample test for the ForSyDeIR which can be used as a starting point.
Modules which are to be tested should be exposed in the library so they are
visible to the test suite and can be imported. The test for the module should
be in the same path in `test` as the module is in `src` and have `Spec`
appended to the end. If this is the case, hspec-discover will automatically
run them when invoked with `cabal new-test` or `cabal test`. The Hspec
documentation recommends passing `--test-show-details=direct` when using cabal.
When run, it should show something like this:
```
...
Running 1 test suites...
Test suite spec: RUNNING...

ForSyDeIR
  IR pretty-printing
    Test hand-crafted IRSystem [✔]
    Empty IR-system should not be an empty string [✔]

Finished in 0.0004 seconds
2 examples, 0 failures
Test suite spec: PASS
Test suite logged to: ...
...
```

In many cases, equality testing with `shouldBe` will be enough for basics, but
[hspec-expectations](https://hspec.github.io/expectations.html) lists a few other
which can also be useful.

Using the QuickCheck integration is described in [hspec-quickcheck](https://hspec.github.io/quickcheck.html).
