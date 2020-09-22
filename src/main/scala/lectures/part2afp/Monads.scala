package lectures.part2afp

object Monads extends App {

  trait Attempt[+A]{
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }

  }

  case class Success[+A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = ???
  }

  /*
  left-identity

  unit.flatMap(f)
    attempt(x).flatMap(f) = f(x)
    Success(x).flatMap(f) = f(x) // proved.

  right-identity

  Attempt(x).flatMap(x => Attempt(x)) = Attempt(x)
  Succes(x).flatMap(x => Attempt(x)) = Attempt(x) = Success(x)
  Fail(_).flat(...) = Fail(e)

  associativity

  attempt.flatMap(f).flatMap(g) = attempt.flatMap(x => f(x).FlatMap(g))

  Fail(e).flatMap(f).flatMap(g) = Fail(e)
  Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)

  Success(v).flatMap(f).flatMap(g) =
    f(v).flatMap(g) OR Fail(e)

  Success(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g) OR fail(e)



   */

}
