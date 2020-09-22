package part3ccp

object Intro extends App {

  // JVM threads

  val aThread = new Thread (new Runnable {
      override def run(): Unit = {
        Thread.sleep(1000)
        println("Hey, this thread is over")
      }
    })


  aThread.start()

  println("This is from the main class")

//  aThread.join()

  Thread.sleep(10)
  println("Let's do some tests.")
}
