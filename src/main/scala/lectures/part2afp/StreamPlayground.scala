package lectures.part2afp

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def filter(predicate: A => Boolean): MyStream[A]

  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B]
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A] = take(n).toList()
  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc
    else tail.toList(head :: acc)
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException
  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def #::[B >: Nothing](element: B): MyStream[B] = new Cons[B](element, this)
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream
  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(f: Nothing => Boolean): MyStream[Nothing] = this
  def take(n: Int): MyStream[Nothing] = this
}

class Cons[+A](hd: A, t: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false
  // Revaluate def as a val so you can reuse it on the body:
  override val head: A = hd
  override lazy val tail: MyStream[A] = t // combining call by name with call by lazyval = call by need

  def #::[B >: A](element: B): MyStream[B] = new Cons[B](element, this)
  // Concatenation still preserves the lazy evaluation.
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons[B](this.head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  /*
  s = new Cons (1,?)
  s.map(_ + 1) = (2, s.tail.map(x => x + 1)) -> which means that the tail is still preserved as lazily,
  unless I call the tail if needed.
   */
  def map[B](f: A => B): MyStream[B] = new Cons[B](f(head),tail.map(f))

  // The "++" should preserve the lazy evaluation, otherwise, "++" will always evaluate the
  // argument ("tail.flatMap") beforehand.
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate:  A => Boolean): MyStream[A] = {
    if (predicate(head)) new Cons(head,tail.filter(predicate))
    else tail.filter(predicate) //preserves lazy evaluation
  }

  def take(n: Int): MyStream[A] =
    if (n == 0) EmptyStream
    else new Cons[A](head, tail.take(n-1)) // still be lazily evaluated

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons(start, MyStream.from(generator(start))(generator))
}


object StreamPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals

  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x+1, EmptyStream))).take(10).toList())
  println(startFrom0.filter(_ % 2 == 0).take(10).toList())

  /*
  Exercises on streams
  1. stream of Fibonacci numbers
  2. stream of prime numbers with Eratosthenes' sieve
  (
   */ def fibonacci(stream: MyStream[Int]): MyStream[Int] = {
    if (stream.head == 1) 1 #:: new Cons[Int](2, fibonacci(stream.tail))
    else stream.flatMap(x => new Cons[Int](stream.head + stream.tail.head,fibonacci(stream.tail)))
  }
  val fibonacci: List[Int] = fibonacci(naturals).take(10).toList()

  println(fibonacci)

}
