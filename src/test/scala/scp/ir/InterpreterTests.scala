package scp
package ir2

import org.scalatest.FunSuite

import MacroTesters._

class InterpreterTests extends FunSuite {
  object b extends ir2.SimpleAST
  
  val Inter = new ir2.BaseInterpreter
  
  def same[A](xy: (A, A)) = assert(xy._1 == xy._2)
  def runSame[A](xy: (b.Rep, A)) = {
    same(b.reinterpret(xy._1, Inter), xy._2)
  }
  
  test("Constants") ({
    
    runSame( shallowAndDeep(b){ 42 } )
    
    runSame( shallowAndDeep(b){ "ok" } )
    
    runSame( shallowAndDeep(b){ 'c' } )
    
    runSame( shallowAndDeep(b){ 'Cool } )
    
  })
  
  test("Basic") {/*Inter.debugFor*/{
    
    runSame( shallowAndDeep(b){ "ok".reverse } )
    runSame( shallowAndDeep(b){ "ok".take(1)+"ko" } )
    runSame( shallowAndDeep(b){ {0 -> 1} swap } )
  
  }}
  
  test("Bindings") {
    
    runSame( shallowAndDeep(b){ val x = 0; x + 1 } )
    runSame( shallowAndDeep(b){ ((x: Int) => x + 1)(42) } )
    runSame( shallowAndDeep(b){ {x: Int => x + 1}.apply(42) } )
    
  }
  
  test("Variables") {
    
    runSame( shallowAndDeep(b){ lib.Var(0) } )
    runSame( shallowAndDeep(b){ var x = ("ok" + "ko".reverse).length; x-=1; (x+=1, x, 'lol) } )
    
  }
  
  test("By-name") {{
    
    runSame( shallowAndDeep(b){ Dummies.byNameMethod(42) } )
    
    runSame( shallowAndDeep(b){ Dummies.byNameMethod(666) } )
    
  }}
  
  test("Varargs") {
    
    runSame( shallowAndDeep(b){ lib.Imperative()(42) } )
    runSame( shallowAndDeep(b){ var x = 0; lib.Imperative(x += 1)(x) } )
    runSame( shallowAndDeep(b){ var x = 0; lib.Imperative(x += 1, x += 1)(x) } )
    runSame( shallowAndDeep(b){ var x = 0; val modifs = Seq(x += 1, x += 1); lib.Imperative(modifs: _*)(x) } )
    
  }
  
  test("Virtualized Constructs") {
    
    // Ascription
    runSame( shallowAndDeep(b){ (List(1,2,3) : Seq[Any]).size: Int } )
    runSame( shallowAndDeep(b){ "ok".length: Unit } )
    
    // If then else
    runSame( shallowAndDeep(b){ if (Math.PI > 0) "ok" else "ko" } )
    runSame( shallowAndDeep(b){ var x = 0; if (true) x += 1 else x += 1; x } )
    
    // While
    runSame( shallowAndDeep(b){ var x = 0; while (x < 10) { x += 1; println(x) }; x } )
    
  }
  
  test("Java") {{
    
    // overloading
    runSame(shallowAndDeep(b){ "ok".indexOf('k'.toInt) }) 
    runSame(shallowAndDeep(b){ "ok".indexOf('k') }) 
    runSame(shallowAndDeep(b){ "okok".indexOf("ok") }) 
    runSame(shallowAndDeep(b){ "okok".lastIndexOf("ok") }) 
    runSame( shallowAndDeep(b){ String.valueOf(true) } )
    
    // workaround for 2-way cache compiler bug (associates java.lang.String to 'object String' instead of 'class String')
    runSame( shallowAndDeep(b){ "ok"+String.valueOf("ko") } )
    runSame( shallowAndDeep(b){ ("ko"*2) })
    runSame( shallowAndDeep(b){ ("ok" + "ko"*2).length })
    
  }}
  
  
  
}














