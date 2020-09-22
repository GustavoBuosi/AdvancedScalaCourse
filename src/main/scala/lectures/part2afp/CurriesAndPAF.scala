package lectures.part2afp

import scala.annotation.tailrec

object CurriesAndPAF extends App {

  // curried functions

  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3)

  println(add3(5))
  println(superAdder(3)(5)) //curried functions
  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method
  // Needs the type annotation, otherwise compiler complains about no. of arguments
  val add4: Int => Int = curriedAdder(4) // Converted a method into a function
  // What was done here was a "lifting" - ETA-EXPANSION

  // functions != methods (JVM limitations)

  def inc(x: Int): Int = x + 1

  List(1,2,3).map(inc) // ETA-expansion -> x => inc(x)

  // Partial function applications: "_" at the
  // end will convert that method into a fuction
  val add5 = curriedAdder(5) _

  // EXERCISE
  val simpleAddFunction: (Int , Int) => Int = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y (function value)

  val curriedAddMethod7 = curriedAddMethod(7) _

  println(curriedAddMethod7(3))

  def simpleAddFunctionBreaker(f: (Int , Int) => Int): Int => Int = x => f(7,x)
  val add7_6 = simpleAddFunction(7, _: Int)
  val add7: Int => Int = (x: Int) => simpleAddFunction(7,x)
  val add7_2 = simpleAddFunction.curried(7)
  val simpleAddFunction7 = simpleAddFunctionBreaker(simpleAddFunction)
  val add7_5 = simpleAddMethod(7, _ : Int)
  println(add7_5(3))

  println(simpleAddFunction7(3))

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName= concatenator("Hello, I'm ", _:String, ", how are you?")

  println(insertName("Daniel"))

  val fillInTheBlanks = concatenator("Hello, I'm ", _: String, _: String)

  println(fillInTheBlanks("Daniel", ". Scala is awesome!"))

  // EXERCISES
  /*
  1. Process a list of numbers and return their
  String representation with different formats.
   */

  def getFormatedList(format: String, list: List[Double]): String = {
    def helper(numbers: List[Double]) = {
      if (!list.isEmpty) format.format(list.head) + ";" + getFormatedList(format,list.tail)
      else ""
    }

    return helper(list)
  }

  println("%4.2f".format(Math.PI))


    val formated = getFormatedList("%4.2f", List(3.14,5.55,0.333333333))
//
  println(formated)

  /*
  With curried functions, this is much more simpler!
   */

  def formaterCurried(s: String)(d: Double)  = s.format(d)

  val formatedList = List(3.14,4.44).map(formaterCurried("%4.2f"))

  val simpleFormatter: Double => String = formaterCurried("%4.2f") _

  println(formatedList)

  println(List(3.14,4.44).map(simpleFormatter))
  println(List(3.14,4.44).map(formaterCurried("%4.2f"))) // compiler does sweet eta-expansion for us
  /*
  2. Differences between
    - functions vs methods
    - parameters: by-name vs 0-lambda
   */

  def byName(n: => Int) = n + 1

  def byFunction(f: () => Int) = f() + 1

  println(byName(2))
  println(byFunction(() => 2))
  def method: Int = 42

  def parenMethod(): Int = 42


  /*
  calling byName and byFunction
  - int
  - method
  - parenMethod
  - lambda
  - PAF
   */

  // NOK byFunction(42)
  // NOK byFunction(method) , no ETA-EXPANSION here!!
  byFunction(parenMethod) // ok, because parenMethod will call "()", which will then be converted to a Int - ETA CONVERSION
  byFunction(()=>42)
  // NOK, since you are evaluating a Int here byFunction((()=>42)())
  byFunction(parenMethod _)


  byName(2)
  byName(method) // ok, because method is evaluated to its call
  byName(parenMethod) // ok, but == byName(parenMethod()), it does not use the function itself.
  // BYNAME AND FUNCTION PARAMETERS ARE COMPLETELY DIFFERENT
  // Does not work, type parameters are different! byName(() => 42)
  byName((()=>42)()) // ok, you defined a PAF and called it to evaluate the 42.
  // NOK as parenMethod _ is evaluated as a function  byName(parenMethod _)


}
