package lectures.part1av

object AdvancedPatternMatching extends App {

  val numbers = List(1)

  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
  }

  /*
  - constants
  - wildcards
  - case classes
  - tuples
  - some special magic like above
   */
  // What if some reason we could not make this class
  // a case class. How can we make it compatible
  // with pattern matching?
  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name,person.age))

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  // Person here associates with the object name!
  // If we changed the object name maintaining the
  // Person type in unapply this would
  // still work.
  val greeting = bob match {
    case Person(n,a) => s"Hi, my name is $n and I am $a years old"
  }

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(greeting)
  println(legalStatus)

  /*
    Exercise.
   */

  object even {
    def unapply(arg: Int): Option[Boolean] =
      if (arg % 2 == 0) Some (true) else None
  }

  object singleDigit {
    def unapply(arg: Int): Option[Boolean] =
      if (arg > -10 && arg < 10) Some(true) else None
  }

  // Design a pattern matching solution for this:
  val n: Int = 45
  val mathProperty =  n match {
  case singleDigit(_) => "single digit"
  case even(_) => "an even number"
  case _ => "no property"
  }

  // infix patterns

  case class Or[A,B](a: A, b: B) // Either
  val either = Or(2, "two")
  val humanDescription = either match {
  //  case Or(number, string) => s"$number is written as $string"
    case number Or string => s"$number is written as $string"
  }
  // decomposing sequences. _* is called a vararg param
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A,
                      override val tail: MyList[A]) extends MyList[A]

  // What if we wanted to pattern match our MyList with the
  // notation _* vararg? Creating an unapplySeq method.
  // Notice that "Seq[A]" is the type returned by the
  // pattern matching.
  // :: won't work for Seqs, but +: will!
//  object MyList {
//    // This turns MyList[A] into an Option[Seq[A]]
//    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
//      if (list == Empty) Some(Seq.empty)
//      else unapplySeq(list.tail).map(list.head +: _)
//  }


  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val list = List(Map(1->2)).flatten.toMap

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1,2, _*) => "starting with 1,2"
    case _ => "something else"
  }

  println(decomposed)

  // custom return types for unapply: it does not
  // need to necessarily return Option. It just
  // needs to implement:
  // isEmpty: Boolean and get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

object PersonWrapper {
  def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
    def get = person.name
    def isEmpty = false
  }
}

  println(bob match {
    case PersonWrapper(n) => s"This person's name is $n"
    case _  => "Allien"
  })

}
