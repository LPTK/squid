// Copyright 2020 EPFL DATA Lab (data.epfl.ch)
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
package classlift

import squid.quasi._

// @dbg_lift
@lift
class MyFatClass {
  
  // The goal is to stress-test the @lift macro.
  // Since it generates loads of reflection code, we used to get a "Platform restriction: a parameter list's length cannot exceed 254"
  // due to a generated class capturing many local generated variables, which were made into constructor parameters after lambda lifting.
  // Now, we still get "Method too large" errors if the generated code is too large.
  
  // Code used for generation:
  //  for (i <- 2 to 22; j <- 1 to i) println((1 to i).mkString("(", ",", ")") + "._" + j)
  
  def foo: Unit = {
    
    (1,2)._1
    (1,2)._2
    (1,2,3)._1
    (1,2,3)._2
    (1,2,3)._3
    (1,2,3,4)._1
    (1,2,3,4)._2
    (1,2,3,4)._3
    (1,2,3,4)._4
    (1,2,3,4,5)._1
    (1,2,3,4,5)._2
    (1,2,3,4,5)._3
    (1,2,3,4,5)._4
    (1,2,3,4,5)._5
    (1,2,3,4,5,6)._1
    (1,2,3,4,5,6)._2
    (1,2,3,4,5,6)._3
    (1,2,3,4,5,6)._4
    (1,2,3,4,5,6)._5
    (1,2,3,4,5,6)._6
    (1,2,3,4,5,6,7)._1
    (1,2,3,4,5,6,7)._2
    (1,2,3,4,5,6,7)._3
    (1,2,3,4,5,6,7)._4
    (1,2,3,4,5,6,7)._5
    (1,2,3,4,5,6,7)._6
    (1,2,3,4,5,6,7)._7
    (1,2,3,4,5,6,7,8)._1
    (1,2,3,4,5,6,7,8)._2
    (1,2,3,4,5,6,7,8)._3
    (1,2,3,4,5,6,7,8)._4
    (1,2,3,4,5,6,7,8)._5
    (1,2,3,4,5,6,7,8)._6
    (1,2,3,4,5,6,7,8)._7
    (1,2,3,4,5,6,7,8)._8
    (1,2,3,4,5,6,7,8,9)._1
    (1,2,3,4,5,6,7,8,9)._2
    (1,2,3,4,5,6,7,8,9)._3
    (1,2,3,4,5,6,7,8,9)._4
    (1,2,3,4,5,6,7,8,9)._5
    (1,2,3,4,5,6,7,8,9)._6
    (1,2,3,4,5,6,7,8,9)._7
    (1,2,3,4,5,6,7,8,9)._8
    (1,2,3,4,5,6,7,8,9)._9
    (1,2,3,4,5,6,7,8,9,10)._1
    (1,2,3,4,5,6,7,8,9,10)._2
    (1,2,3,4,5,6,7,8,9,10)._3
    (1,2,3,4,5,6,7,8,9,10)._4
    (1,2,3,4,5,6,7,8,9,10)._5
    (1,2,3,4,5,6,7,8,9,10)._6
    (1,2,3,4,5,6,7,8,9,10)._7
    (1,2,3,4,5,6,7,8,9,10)._8
    (1,2,3,4,5,6,7,8,9,10)._9
    (1,2,3,4,5,6,7,8,9,10)._10
    (1,2,3,4,5,6,7,8,9,10,11)._1
    (1,2,3,4,5,6,7,8,9,10,11)._2
    (1,2,3,4,5,6,7,8,9,10,11)._3
    (1,2,3,4,5,6,7,8,9,10,11)._4
    (1,2,3,4,5,6,7,8,9,10,11)._5
    (1,2,3,4,5,6,7,8,9,10,11)._6
    (1,2,3,4,5,6,7,8,9,10,11)._7
    (1,2,3,4,5,6,7,8,9,10,11)._8
    (1,2,3,4,5,6,7,8,9,10,11)._9
    (1,2,3,4,5,6,7,8,9,10,11)._10
    (1,2,3,4,5,6,7,8,9,10,11)._11
    (1,2,3,4,5,6,7,8,9,10,11,12)._1
    (1,2,3,4,5,6,7,8,9,10,11,12)._2
    (1,2,3,4,5,6,7,8,9,10,11,12)._3
    (1,2,3,4,5,6,7,8,9,10,11,12)._4
    (1,2,3,4,5,6,7,8,9,10,11,12)._5
    (1,2,3,4,5,6,7,8,9,10,11,12)._6
    (1,2,3,4,5,6,7,8,9,10,11,12)._7
    (1,2,3,4,5,6,7,8,9,10,11,12)._8
    (1,2,3,4,5,6,7,8,9,10,11,12)._9
    (1,2,3,4,5,6,7,8,9,10,11,12)._10
    (1,2,3,4,5,6,7,8,9,10,11,12)._11
    (1,2,3,4,5,6,7,8,9,10,11,12)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._1
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._2
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._3
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._4
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._5
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._6
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._7
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._8
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._9
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._10
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._11
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13)._13
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._1
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._2
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._3
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._4
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._5
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._6
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._7
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._8
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._9
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._10
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._11
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._13
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14)._14
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._1
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._2
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._3
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._4
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._5
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._6
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._7
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._8
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._9
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._10
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._11
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._13
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._14
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)._15
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._1
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._2
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._3
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._4
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._5
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._6
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._7
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._8
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._9
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._10
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._11
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._13
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._14
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._15
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)._16
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._1
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._2
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._3
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._4
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._5
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._6
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._7
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._8
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._9
    /* // Beyond this point, we get the error: "Method too large: squid/classlift/MyFatClass$cls$1$.<init> (Lsquid/lang/Definitions;Lscala/runtime/LazyRef;Lscala/runtime/LazyRef;Lscala/runtime/LazyRef;)V"
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._10
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._11
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._13
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._14
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._15
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._16
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)._17
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._1
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._2
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._3
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._4
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._5
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._6
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._7
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._8
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._9
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._10
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._11
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._13
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._14
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._15
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._16
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._17
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)._18
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._1
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._2
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._3
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._4
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._5
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._6
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._7
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._8
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._9
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._10
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._11
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._12
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._13
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._14
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._15
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._16
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._17
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._18
    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)._19
    */
    
  }
  
}