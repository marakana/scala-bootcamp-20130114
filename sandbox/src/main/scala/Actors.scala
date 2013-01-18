import akka.actor.{Props, ActorSystem, ActorRef, Actor}
import akka.pattern.ask
import akka.util.Timeout
import concurrent.Await
import concurrent.duration._
import util.Random

object Actors {

  def fib(n: Int): BigInt = n match {
    case 0 | 1 => 1
    case _ => fib(n-1) + fib(n-2)
  }

  case object Run
  class Fib extends Actor {
    def receive = {
      case Run => sender ! fib(Random.nextInt(40))
    }
  }

  class Main extends Actor {
    import context.dispatcher
    implicit val timeout = Timeout(5 seconds)
    def receive = {
      case Run =>
        val product = for {
          a <- (context.actorOf(Props[Fib]) ? Run).mapTo[BigInt]
          b <- (context.actorOf(Props[Fib]) ? Run).mapTo[BigInt]
          c <- (context.actorOf(Props[Fib]) ? Run).mapTo[BigInt]
        } yield a * b * c
        println(Await.result(product, timeout.duration))
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem()
    val main = system.actorOf(Props[Main])
    main ! Run
    system.shutdown()
  }

}
