package lectures.part2afp

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  /*
  Sets are callable because they have an apply method.
  Sets then behave like functions!

  trait Set[A] extends (A) => Boolean with...

  Treating a well known collection as a function
  is a core of Functional Programming.
   */

  // 1. define an apply method
  def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]


  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(f: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
  Exercise:
  - removing an element (operator)
  - intersection with another set (operator)
  - difference with another set
   */
  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A) = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(f: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()
  def -(elem: A): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this
  def &(anotherSet: MySet[A]): MySet[A] = this
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

// {x in A | property(x)}
// all elements of type A which satisfy a property
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  def contains(elem: A): Boolean = property(elem)
  def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x))
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))
  def foreach(f: A => Unit): Unit = politelyFail

  /*
  Exercise:
  - removing an element (operator)
  - intersection with another set (operator)
  - difference with another set
   */
  def -(elem: A): MySet[A] = filter(x => x != elem)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole")
}


class NonEmptySet[A](val head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A) =
    head == elem || tail.contains(elem)
  def +(elem: A): MySet[A] = {
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)
  }

  /*
  We try to write the non-empty class as close as possible to a
  functional syntax.
   */

  def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head
  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ (tail flatMap f)
  def filter(f: A => Boolean): MySet[A] = {
    if (f(head)) tail.filter(f) + head
    else tail.filter(f)
  }
  def foreach(f: A => Unit): Unit = {
    tail.foreach(f)
    f(head)
  }


  def -(elem: A): MySet[A] = filter(x => x != elem)
  // We could implement an unary operator here if we wanted to here...
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  // Since anotherSet is also a function, we can reduce the expression:
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  // Implementing a unary negation:

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))

}

// Companion object to build sets:
object MySet {
  /*
  I can provide multiple values of type A using the "A*" syntax
  val s = MySet(1,2,3) = buildSet(Seq(1,2,3),new EmptySet[A])
   */
  def apply[A](values: A*): MySet[A] = {
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, new EmptySet[A])
  }

}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1,-2) + 3 flatMap  (x => MySet(x, 10*x)) foreach println

  val negative = !s // s.unary_! = all the naturals NEQ to 1,2,3 or 4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))

}