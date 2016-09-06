package scp
package ir2

import utils._
import lang2._

/** Transformer that applies the rewrite rules repeatedly until a fixed point is reached or `MAX_TRANSFORM_ITERATIONS` is exceeded. */
trait FixPointRuleBasedTransformer extends SimpleRuleBasedTransformer {
  val base: InspectableBase
  
  import base._
  import TranformerDebug.debug
  
  val MAX_TRANSFORM_ITERATIONS = 8
  
  override def transform(rep: Rep) = {
    var matched = true
    var currentRep = rep
    var recNum = 0
    
    while (matched && recNum < MAX_TRANSFORM_ITERATIONS) {
      //debug(s" --- ($recNum) --- ")
      
      recNum += 1
      matched = false
      
      rules foreach { case (xtor, code) =>
        //debug(s"Matching xtor ${xtor.show} << ${currentRep.show}")
        
        extract(xtor, currentRep) foreach { ex => try {
          debug(s"Got Extract: $ex")
          
          val resOpt = code(ex)
          debug(s"Got Code: ${resOpt map (_ show)}")
          
          resOpt foreach { res =>
            currentRep = res
            matched = true
          }
        } catch {
          case RewriteAbort(msg) =>
            debug(s"Rewrite aborted. " + (if (msg isEmpty) "" else s"Message: $msg"))
        }}
      }
    }
    
    if (recNum == MAX_TRANSFORM_ITERATIONS) System.err.println(s"Online rewrite rules did not converge after $MAX_TRANSFORM_ITERATIONS iterations.")
    //debug(" --- END --- ")
    
    currentRep
  }
  
}



