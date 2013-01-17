import java.util
import java.util.concurrent.{TimeUnit, Executors}
import scala.util.Random

object Actors {

  def fib(n: Int): BigInt = n match {
    case 0 | 1 => 1
    case _ => fib(n-1) + fib(n-2)
  }

  class Logger extends Runnable {
    private val messages: util.Queue[String] = new util.LinkedList[String]()
    def log(message: String): Unit = synchronized {
      messages.add(message)
      notify()
    }

    def run = while (true) {
      synchronized {
        if (!messages.isEmpty)
          println(messages.remove())
        else
          wait()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val service = Executors.newFixedThreadPool(10)
    val logger = new Logger
    service.execute(logger)
    for (i <- 1 to 100) {
      service.execute(new Runnable {
        def run = logger.log(fib(Random.nextInt(40)).toString)
      })
    }
    service.shutdown()
    service.awaitTermination(1, TimeUnit.SECONDS)
  }

}