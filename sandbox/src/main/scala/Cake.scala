object Cake {

  trait Show[A] {
    def show(value: A): String
  }

  trait Logger {
    def logLevel: Int
    def log[A : Show](message: A): A
  }

  trait ConsoleLogger extends Logger {
    def log[A : Show](message: A): A = {
      println(implicitly[Show[A]].show(message))
      message
    }
  }

  trait DataSource {
    def request[A](query: String): A
  }

  trait TerribleDataSource extends DataSource {
    def request[A](query: String): A = ???
  }

  trait Configuration { this: Logger with DataSource =>
    def lookSomethingUp[A](query: String): A = {
      val result = request(query)
      log(result)
    }
  }

  new Configuration with ConsoleLogger with TerribleDataSource {
    val logLevel = 3
  }
}
