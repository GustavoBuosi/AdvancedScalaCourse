package lectures.part1av

import scala.annotation.tailrec

object recap extends App {

  def aFunction(x: Int): Int = x + 1

  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0 ) accumulator
    else factorial (n - 1, n * accumulator)

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch")
  }

  val aCroc = new Crocodile
  aCroc eat aDog

  val sum = 1.+(2)

  // anonymous classes:

  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  abstract class MyList[+A] //variance and variance problems
  // singletons and companions

  object MyList

  // case classes: serializable, all parameters are fields,
  // all parameters have apply methods etc.

  case class Person(name: String, age: Int)

  // exceptions and try/catch/finally

  val throwsException: Nothing = throw new RuntimeException

  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "I caught an exception"
  } finally {
    println("some logs")
  }

  // packaging and imports

  // functional programming
  // Functions are instances of classes with apply methods

  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1

  List(1,2,3).map(anonymousIncrementer)

  //map, flatMap and filter: basics for for-comprehension

  val pairs = for {
    num <- List(1,2,3) if num %2 == 0
    char <- List("a","b","c")
  } yield num + "-" + char

  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  // "collections": Options, Try

  val anOption = Some(2)

  val x = 2

  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }

  // all the patterns

}
