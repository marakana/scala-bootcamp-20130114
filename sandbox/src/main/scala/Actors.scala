import java.util
import util.concurrent.{LinkedBlockingQueue, BlockingQueue, TimeUnit, Executors}
import scala.util.Random

object Actors {

  def fib(n: Int): BigInt = n match {
    case 0 | 1 => 1
    case _ => fib(n-1) + fib(n-2)
  }

  trait Actor extends Runnable {
    private val messages: BlockingQueue[Any] = new LinkedBlockingQueue()
    def !(message: Any): Unit = messages.put(message)
    def run = while (true)
      receive(messages.take())

    def receive(message: Any): Unit
  }

  def main(args: Array[String]): Unit = {
    val service = Executors.newFixedThreadPool(10)
    val logger = new Actor {
      def receive(message: Any) = println(message.toString)
    }
    service.execute(logger)
    for (i <- 1 to 100) {
      service.execute(new Runnable {
        def run = logger ! (fib(Random.nextInt(40)).toString)
      })
    }
    service.shutdown()
    service.awaitTermination(1, TimeUnit.SECONDS)
  }

}
