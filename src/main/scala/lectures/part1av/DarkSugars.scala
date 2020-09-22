package lectures.part1av

import scala.util.Try

object DarkSugars extends App {

  // syntax sugar #1: methods with single param

  def singleArgMethod(arg: Int): String = s"$arg little ducks..."

  val description = singleArgMethod {
    // write some complex code
    42
  }

  val aTryInstance = Try { // java's try {...}
    throw new RuntimeException
  }

  List(1,2,3).map{
    x => x + 1
  }

  // syntax sugar #2: single abstract method pattern

  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  // Only available after Scala 2.12, otherwise it won't resolve:
  val aFunkyInstance: Action = (x: Int) => x + 1 //magic

  // runnable: java interfaces / traits
  // that can be passed to threads

  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello, Scala")
  })

  // Only available after Scala 2.12, otherwise it won't resolve:
  val aSweeterThread = new Thread( () => println("sweet, Scala!") )

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  // This is just syntatic sugar, it is not implicits!
  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // :: and #:: are special operators

  val prependedList = 2 :: List(3,4) // = List(3,4).::(2)

  // scala spec: last char decides associativity of method
  // If it ends in a column (:), it is right associative.
  // That is why List(3,4) :: 2 wont't compile


  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  /*
  multi-word method naming
   */

  class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")

  lilly `and then said` "Scala is so sweet!"

  // inflix types

  class Composite[A,B]
  val composite: Composite[Int, String] = ???
  val compositeDifferente: Int Composite String = ???

  class -->[A,B]
  val towards: Int --> String = ???

  // update method, special like apply

  val anArray: Array[Int] = Array(1,2,3)

  anArray(2) = 7 // rewriten to anArray.update(2,7)
  // used in mutable collections
  // remember apply() AND update()

  // setters for mutable containers

  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member = internalMember // "getter"
    def member_=(value:Int): Unit =
      internalMember = value
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42

}
