package graph4

import squid.utils._
import org.scalatest.FunSuite

class InterpTests extends FunSuite {
  object TestHarness extends TestHarness
  import CheckDSL.check
  
  // TODO recover the simple PE'd programs we had with old scheduling, and not these big flag-filled monstrosities
  
  test("InterpSimple") (
    // TODO test version with two params and a param cycle (graph used to diverge)
    TestHarness("InterpSimple",
      //dumpGraph = true,
    )(
      check('test)(497)
    )
  )
  
  test("InterpTrivial") (
    TestHarness("InterpTrivial",
      //prefixFilter = "test1",
      dumpGraph = true,
    )(
      check('test3)(127)
    )
  )
  
  test("InterpTrivialRec") (
    // TODO counteract scheduled code regression since <this commit>, due to unrolling up to UnrollingFactor
    TestHarness("InterpTrivialRec",
      //prefixFilter = "test1",
      dumpGraph = true,
    )(
      // Note: wrong schedule when using ps.last instead of ps.head...
      check('test2_10)(List(true,true,false,true,true,false,true,true,false,true)),
      check('test3_10)(List(true,true,true,false,true,true,true,false,true,true)),
    )
  )
  
}
