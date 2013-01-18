import java.util
import util.concurrent.{LinkedBlockingQueue, BlockingQueue, TimeUnit, Executors}
import scala.util.Random

object Actors {

  def fib(n: Int): BigInt = n match {
    case 0 | 1 => 1
    case _ => fib(n-1) + fib(n-2)
  }

  class Logger extends Runnable {
    private val messages: BlockingQueue[String] = new LinkedBlockingQueue()
    def log(message: String): Unit = messages.put(message)
    def run = while (true)
      println(messages.take())
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
