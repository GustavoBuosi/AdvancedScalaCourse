package lectures.part2afp

object LazyEvaluation extends App {

  lazy val x : Int = {
    println("hello")
    42
  }

  // Evaluated once, only when used for the first time
  // lazy delays the evaluation!

  println(x)
  // It wont't be evaluated here again.
  println(x)

  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  // Because simpleCondition is false, lazyCondition is never evaluated at this stage!
  println(if (simpleCondition && lazyCondition) "yes" else "no")
  // However:
  val anotherSimpleCondition = true
  println(if (anotherSimpleCondition && lazyCondition) "yes" else "no")

  // in conjuction with call by name

  def byNameMethod(n: => Int): Int  = {
    lazy val t = n // I only want it to evaluate here
    t + t + t + 1
  }

  def retrieveMagicValue = {
    println("Waiting")
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))

  // filtering with lazy vals

  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30 _)
  val gt20 = lt30.filter(greaterThan20 _)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30 _) // lazy vals under the hood
  val gt20lazy = lt30lazy.withFilter(greaterThan20 _)

  // withFilter does not get evaluated here, so this will print cryptic name
  println(gt20lazy)
  // In order to force:
  // THE PREDICATES ARE CHECKED ON A PER NEED BASIS!
  gt20lazy.foreach(println)

  // for-compreshensions uses withFilter with guards!

  for {
    a <- List(1,2,3) if a % 2 == 0 // "if" uses lazy vals
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1)



}
