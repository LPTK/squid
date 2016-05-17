package scp
package feature

object BasicEmbedding {
  
  case class MC[A](x:Int)(syms: A*)
  
  def foo(n: Int)(s: String) = s * n
  
}
class BasicEmbedding extends MyFunSuite {
  import BasicEmbedding._
  
  import TestDSL._
  
  test("Trivial") {
    
    dsl"42" matches {
      case dsl"42" =>
    }
    dsl"42.toDouble + .5" matches {
      case dsl"42.toDouble + 0.5" =>
    }
    
  }
  
  test("Static objects and classes (java.lang.Math, String)") {
    
    val pow23 = dsl"Math.pow(2, 3)"
    
    pow23 match {
      case dsl"java.lang.Math.pow(2,3)" =>
    }
    pow23 match {
      case dsl"java.lang.Math.pow(${Constant(2)}, ${Constant(3)})" =>
    }
    pow23 match {
      case dsl"java.lang.Math.pow(${Constant(m)}, ${Constant(n)})" =>
        same(m,2)
        same(n,3)
    }
    pow23 match {
      case dsl"java.lang.Math.pow($m, $n)" =>
        eqt(m,dsl"2")
        eqt(n,dsl"3")
    }
    
    dsl"String valueOf true" matches {
      case dsl"java.lang.String.valueOf(true)" =>
    }
    
  }
  
  test("New") {
    
    
    dsl"new MC[Nothing](42)(???)" match {
      //case dsl"new MC(42)()" => // warns
      case dsl"new MC[Nothing](42)()" => fail
      //case dsl"new MC[Nothing](42)($x)" => // warns
      //case dsl"new MC(42)($x:Nothing)" => // warns
      case dsl"new MC[Nothing](42)($x:Nothing)" => // ok
    }
    
    //dbgdsl"new MC(42)('ok, 'ko)" // TODO look at gen'd code and reduce
    val mc = dsl"new MC(42)('ok, 'ko)"
    
    mc matches {
      case dsl"new MC(42)('ok, 'ko)" =>
    } and {
      case dsl"new MC($n)('ok, Symbol($str))" =>
        eqt(n, dsl"42")
        eqt(str, dsl"${"ko"}")
    } and {
      //case dsl"new MC($n)($a, $b)" => fail // should generate a warning
      case dsl"new MC[$t]($n)($a, $b)" =>
    } and {
      case dsl"$mc: MC[$t]" =>
        eqt(t.rep, typeRepOf[Symbol])
        eqt(mc.trep, typeRepOf[MC[Symbol]])
    }
    
    assertTypeError(""" mc match { case dsl"new $ab" => } """) // Embedding Error: trait ab is abstract; cannot be instantiated
    
  }
  
  test("Methods") {
    import collection.mutable.Stack
    
    dsl"Stack[Int](1,42,2).push(0)" matches {
      case dsl"Stack(1,$n,2).push($m)" =>
        n eqt dsl"42"
        m eqt dsl"0"
    }
    
    dsl"Stack(42).map(_+1)" matches {
      case dsl"Stack[$ta]($n).map($f: ta => $tb)" => eqt(f.trep, typeRepOf[Int => Int])
    }
    
    dsl"Stack[Int](42).map(_+1).isEmpty" matches {
      case dsl"Stack[Int](42).map(_+1).isEmpty" => // TODO
    } and {
      case dsl"Stack[$ta]($n).map($f: ta => $tb).isEmpty" =>
        eqt(ta.rep, typeRepOf[Int])
        eqt(tb.rep, typeRepOf[Int])
        eqt(f.typ.rep, typeRepOf[Int => Int])
    }
    
  }
  
  test("Curried Functions") {
    
    val x = dsl"""foo(42)("ok")"""
    
    //x match { case dsl"($f: Int => String)($s)" => println(f) } // nope (normal)
    
    x match {
      case dsl"foo($n)($s)" =>
        assert(n =~= dsl"42")
        assert(s =~= dsl""" "ok" """)
    }
    
  }
  
  test("Array's & ClassTag's") {
    // FIXME
    /*
    dsl"""Array.fill(3)("woof")""".erase match {
      case dsl"Array.fill[String]($_)($_)($ev)" => // weird error in the following dsl"": Error:(120, 23) Cannot generate a type representation for: ?0
      //case dsl"Array.fill[$t]($_)($_)($ev)" =>
        //assert(ev =~= dsl"scala.reflect.classTag[String]") // Toolbox classpath problem? >> Could not find 'classTag' in scala.reflect.package
        assert(ev =~= dsl"scala.reflect.ClassTag[String](${"".getClass})")
        //println(ev.run) // works!
    }
    */
  }
  
  test("List, Option") {
    
    dsl"Some(1.2)" matches {
      //case dsl"Some($_)" => fail
      case dsl"Some[Nothing]($_:Nothing)" => fail // equivalent to the one above, but does not generate a warning
      //case dsl"Some[Any]($_)" => fail // method type args are seen as invariant (we maybe could do better but it'd require non-trivial analysis)
      case dsl"Some[Any]($_)" => // now method type args are seen as covariant
    } and {
      case dsl"Some[Double]($_)" =>
    }
    dsl"Option.empty: Option[Double]" match {
      //case dsl"Option.empty[Double]" => fail // FIXME?
      case dsl"$_: Option[Double]" => 
    }
    dsl"Option.empty".erase match {
      //case dsl"Option.empty[Double]" => fail // FIXME?
      case dsl"$_: Option[Double]" => 
    }
    dsl"Option(3.4)" match {
      case dsl"Option[Double]($_)" => 
    }
    
    val ls = dsl"List(Some(1.2),Option(3.4),Option.empty,None)"
    ls match {
      case dsl"List($_,$_,None,None)" => fail
      //case dsl"List(Some[Any]($_),Option[Any]($_),Option.empty[Any],None)" =>
      case dsl"List[Option[Double]](Some[Double]($_),Option[Double]($_),($_:Option[Double]),None)" =>
    }
    ls.erase match {
      case dsl"$_: List[Nothing]" => fail
      case dsl"$_: List[Any]" =>
    }
    ls.erase match {
      case dsl"$ls: List[$t]" => assert(t.rep =:= typeRepOf[Option[Double]])
    }
  }
  
}





