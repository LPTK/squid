
Here is what I was able to gather on 2024-06-08, about 5 years later.


### Latest status

First, the latest implementation seems to be called "graph4" in the code and tests.
The older "haskellopt" implementation is still there but uses the old "graph3" code;
it generally seems more problematic, and I suppose graph4 cleaned up a bunch or problems with it.
Confusingly, both live in the SBT project called "haskellopt".

For some incomprehensible reason (probably due to a newer version of `ghc-dump-core`),
the Core we get from GHC now seems to include some new nodes,
tagged with constructor ID 9. It seems everything still works if we ignore them,
as I now do in the `case Arr(IntElem(9), ty) =>` case of `Interpreter.scala`.

Note: to make things compile again I used a more recent version of SBT,
disabled the macro-paradise plugin, and commented some code in `ScalaCore.scala`,
so the dependent Squid parts are probably broken.


### Installation

The "graph4" implementation works from this, which was the GHC version used originally:

```
ghcup install ghc 8.6.3
ghcup set ghc 8.6.3
cabal install --lib ghc-dump-core
cabal install --lib criterion
```

It probably also works with more recent versions of GHC 8.

The older "haskellopt" implementation seems to require the "tuple" library, which I was able to install with:

```
ghcup install ghc 8.8.1
ghcup set ghc 8.8.1
cabal install --lib ghc-dump-core
cabal install --lib criterion
cabal install --lib tuple
```


### Running the tests

In SBT, use for example `haskellopt/testOnly graph4.BasicTests -- -z Basic`
to run the `Basic` test case of the `BasicTests` suite in `haskellopt/src/test/scala/graph4`.
This will:
 * shell out to GHC to generate some Core IR dump
 * load the dump into graph IR form
 * perform some rewriting on the graph until all redexes are reduced or some limit is reached
 * "schedule" the graph back into a Haskell program
  (this has been the most difficult, buggy, and problematic part, which we may want to drop, if we focus on program analysis)
 * shell out to GHC to compile the Haskell program and run some checks on it, as well as possibly run it with Criterion to measure its performance

Note: the old "haskellopt" implementation by default performs the above steps twice:
once from the GHC dump after desugaring (`pass-0000`),
and once after the first round of GHC Core simplification (`pass-0001`),
as determined in the `passes: List[String]` of the old `haskellopt.TestHarness`,
which has default value `List("0000", "0001")`.
I suppose this was because both could be interesting to study and may have pros and cons.
In "graph4", it seems we only ever use the `pass-0000` dump.



