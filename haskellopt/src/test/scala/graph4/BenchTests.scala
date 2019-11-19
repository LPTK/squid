package graph4

import squid.utils._
import org.scalatest.FunSuite

class BenchTests extends FunSuite {
  object TestHarness extends TestHarness {
    import ammonite.ops._
    override val genFolder = if (useNewScheduler) pwd/'haskellopt_gen3/'bench else pwd/'haskellopt_gen2
  }
  import CheckDSL.check
  
  test("InterpBench") (
    // FIXME now graph diverges
    //TestHarness("InterpBench",
    //  //dumpGraph = true,
    //)(
    //)
  )
  
  test("InterpIntermediateBench") (
    // Note: periodically make sure result is the same by uncommenting `print`...
    TestHarness("InterpIntermediateBench",
    )(
    )
  )
  
  test("InterpBasicBench") (
    // TODO make it work with bigger `src` now that stack overflow is fixed
    // FIXME towards the end: Maximum propagation depth reached (64)
    TestHarness("InterpBasicBench",
      schedule = !TestHarness.useNewScheduler, // FIXME(NS) bad comparison in toCaptureArg
    )(
    )
  )
  
  test("StatisticsBench") (
    // TODO try with an unboxed return tuple/unboxed argument tuple
    TestHarness("StatisticsBench",
    )(
      check('maxMaybe, List(1,3,2,0))(Some(3)),
    )
  )
  
  test("AvgBench") (
    TestHarness("AvgBench",
    )(
      check('avg, List(1,3,2,0))(1),
    )
  )
  
  test("MaxPEBench") (
    // FIXME when `max_mod` is not explicitly typed: _0 = ($p1Real) ($p1Integral)
    TestHarness("MaxPEBench",
    )(
      //check('maxMaybe, List(1,3,2,0))(Some(3)),
    )
  )
  
  val ls = List(1,2,3,4)
  val lsAvg = ls.sum/ls.size
  
  // Note: its sibling FoldingPolyBench can be optimized, but generates a program where type inference fails due to the forall in Control.Foldl
  test("FoldingBench") (
    // FIXME use the `go` version of `foldl'`, which currently causes a BadComparison in scheduling...
    TestHarness("FoldingBench",
      multiStepReductions = false, // FIXME paths/references seem to get messed up otherwise...
    )(
      check('avg_F, ls)(lsAvg),
      check('avg_manual, ls)(lsAvg),
      //check('avg_manual, Nil)(0),
    )
  )
  
  test("nofib-queens") (
    // Notes about the old scheduler (SmartScheduler):
    //   It raised BadComparison with a crazy scope number: graph4.GraphDefs$BadComparison: <rec'303889>(ds'32:d↑[↓]) `;` β_2e = [from'13:8↑[↓]+1]ψ_30
    //   In general, the scheduling process for programs like these goes off-track even though the graph is fine
    //   — it never seems to finish, even after some simplifications.
    //   With UnrollingFactor == 0 we could manage to finish scheduling (more than 100 lines of generated code),
    //   but the program performed identically to the original.
    //   Similarly, if we removed all case commuting, nothing reduces and we get the same result.
    TestHarness("nofib-queens",
      //dumpGraph = true,
      //prefixFilter = "nsoln",
    )(
      // Note: these are useful but end up taking way too long when the graph gets complicated:
      //check('nsoln, 2)(0),
      //check('nsoln, 4)(2),
    )
  )
  
  test("nofib-gen_regexps") (
    TestHarness("nofib-gen_regexps",
      //dumpGraph = true,
      //prefixFilter = "expand",
      //prefixFilter = "alphabeticRule",
      compileResult = false, // FIXME(NS) needs ScopeAccessMethod.Case to successfully type infer
    )(
    )
  )
  
  test("nofib-digits-of-e1") (
    TestHarness("nofib-digits-of-e1",
    )(
    )
  )
  
  
  // TODO nofib-primes with list fusion (but probably already done by GHC)
  
  
}
