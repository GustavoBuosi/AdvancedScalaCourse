package lectures.part2afp

import scala.util.{Failure, Success, Try}

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1 // Function1[Int,Int] === Int => Int

  /*
  What if we want to restrict the domain to the function?
   */

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  /*

  The below is an implementatiion of the Domain {1,2,5} to Int
  {1,2,5} => Int
  Since {1,2,5} is a subset of Int, we can say that this is a
  partial function of Int => Int
   */

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }


  val aPartialFunction: PartialFunction[Int,Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  println(aPartialFunction(2))
  println(aPartialFunction(5))

  /*
  PF utilities
   */

  println(aPartialFunction.isDefinedAt(67))

  // lift: returns None instead of an exception

  val lifted: Function1[Int,Option[Int]] = aPartialFunction.lift // Int => Option[Int]

  println(lifted(2)) // Some(56)
  println(lifted(98)) // None


  /*
  Chaining partial functions
   */

  val pfChain = aPartialFunction.orElse[Int,Int] {
    case 45 => 67
  }

  println(pfChain(2))
  println(pfChain(45))

  /*
  Partial Functions extend normal functions, so a PF can be
  attributed to a total function
   */

  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  /*
  HOF accept partial functions as well!!!
   */

  val aMappedList = List(1,2,"oi").map {
    case 1 => 42
    case 2 => 78
    case "oi" => 1000
  }

  println(aMappedList)

  /*
  Note: PF can have ONE parameter (type?)
   */

  /*
  1 - construct a PF instance yourself (anonymous class)
  2 - dumb chatbot as a PF
   */

  // 1)

  val myPartialFunction = new PartialFunction[Int,Int] {
    override def isDefinedAt(x: Int): Boolean = Try(apply(x)) match {
      case Success(_) => true
      case Failure(_) => false
    }

    override def apply(v: Int): Int = {
      if (v == 2) 20
      else if (v == 5) 50
      else throw new RuntimeException
    }
  }

  println(myPartialFunction.isDefinedAt(3))
  println(myPartialFunction(2))




}
