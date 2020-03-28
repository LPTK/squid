// Copyright 2017 EPFL DATA Lab (data.epfl.ch)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package squid
package feature

import utils._

class PrettyPrinting extends MyFunSuite {
  import TestDSL.Predef._
  import TestDSL.Quasicodes._
  import TestDSL.showRep
  
  test("Basic Types") {
    
    same(code"Option.empty"
      .rep |> showRep, "scala.Option.empty[scala.Nothing]")
    same(code"Option.empty[Int]"
      .rep |> showRep, "scala.Option.empty[scala.Int]")
    same(code"Option.empty[AnyRef]"
      .rep |> showRep, "scala.Option.empty[java.lang.Object]")
    
  }
  
  type T
  test("Abstract Types") {
    same(code"Option.empty[T]"
      .rep |> showRep, "scala.Option.empty[(squid.feature.PrettyPrinting)#T]")
    same(code"Option.empty[PrettyPrinting.T]"
      .rep |> showRep, "scala.Option.empty[squid.feature.PrettyPrinting.T]")
  }
  
  test("Bindings") {
    
    same(code"(x: Int) => x+1"
      .rep |> showRep, """((x_0: scala.Int) => x_0.+(1))""".stripMargin)
    
    same(code"val x = 123; x+1"
      .rep |> showRep, """{
      |  val x_0 = 123;
      |  x_0.+(1)
      |}""".stripMargin)
    
  }
  
  test("Implicit Bindings") {
    
    same(code"implicit val x = 42; implicitly[Int]+1".rep |> showRep, """{
      |  implicit val x_0 = 42;
      |  scala.Predef.implicitly[scala.Int](x_0).+(1)
      |}""".stripMargin)
    
    same(code"{implicit x: Int => implicitly[Int]+1}".rep |> showRep,
      "(implicit x_0: scala.Int => scala.Predef.implicitly[scala.Int](x_0).+(1))")
    
    // Equivalently, can use annotation of the underlying encoding explicitly:
    same(code"@squid.lib.Implicit val x = 42; implicitly(x)+1".rep |> showRep, """{
      |  implicit val x_0 = 42;
      |  scala.Predef.implicitly[scala.Int](x_0).+(1)
      |}""".stripMargin)
    
  }
  
  test("Null variables") {
    
    same(code"var x: Int = ${nullValue[Int]}; x+1"
      .rep |> showRep, """{
      |  var x_0: scala.Int = 0;
      |  x_0.+(1)
      |}""".stripMargin)
    
    same(code"var x: Int = null.asInstanceOf[Int]; x+1"
      .rep |> showRep, """{
      |  var x_0: scala.Int = null.asInstanceOf[scala.Int];
      |  x_0.+(1)
      |}""".stripMargin)
    
    val err = captureStdErr {
      same(code"var x: Int = ${code"null".asInstanceOf[Code[Int,Any]]}; x+1" 
      // ^ Warning: variable `[x:squid.lib.package.MutVar[Int]]` of type `Int` (not a subtype of `AnyRef`) is assigned `null`.
        .rep |> showRep, """{
        |  var x_0: scala.Int = null;
        |  x_0.+(1)
        |}""".stripMargin)
    }
    assert(err ==
      "Warning: variable `[x:squid.lib.package.MutVar[Int]]` of type `Int` (not a subtype of `AnyRef`) is assigned `null`.\n"
      || err ==
      "Warning: variable `[x:squid.lib.package.MutVar[scala.Int]]` of type `scala.Int` (not a subtype of `AnyRef`) is assigned `null`.\n")
    
    same(code"var x: String = ${nullValue[String]}; x+1"
      .rep |> showRep, """{
      |  var x_0: java.lang.String = null;
      |  x_0.+(1)
      |}""".stripMargin)
    
    same(code"var x: String = null; x+1"
      .rep |> showRep, """{
      |  var x_0: java.lang.String = null;
      |  x_0.+(1)
      |}""".stripMargin)
    
  }
  
  test("Free variables") {
    
    same(code"(?x:Int)+1".toString, """code"?x.+(1)"""")
    
  }
  
  test("First-Class Variable Symbols") {
    
    val v = Variable[Int]
    
    // These are potentially tricky cases demonstrating shadowing of a symbol
    
    val str = """code"((v_0: scala.Int) => ((v_1: scala.Int) => v_1.+(1)))""""
    same(code"($v:Int) => ($v:Int) => ($v:Int)+1".toString, str)
    same(code"($v => $v => $v+1)".toString, str)
    
    // FIXME pretty-printing of shadowed bindings:
    //same(code"{$v => ($v => $v+1, $v)}".toString, 
    //  """code"((v_0: scala.Int) => scala.Tuple2.apply[scala.Function1[scala.Int, scala.Int], scala.Int](((v_1: scala.Int) => v_1.+(1)), x_0))""""")
    
  }
  
  test("Tricky Types") {
    import TrickyTypes._
    
    same(code"Option.empty[ModularMetaprog#Foo[Int]]".rep |> showRep,
      "scala.Option.empty[(squid.feature.TrickyTypes.ModularMetaprog)#Foo[scala.Int]]")
    
  }
  
}
object PrettyPrinting {
  type T
}
