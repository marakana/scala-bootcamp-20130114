object Monads {

  sealed trait Option[+A] {
    def map[B](fn: A => B): Option[B]
  }
  object Option {
    case class Some[+A](value: A) extends Option[A] {
      def map[B](fn: A => B): Option[B] = ???
    }
    case object None extends Option[Nothing] {
      def map[B](fn: Nothing => B): Option[B] = ???
    }
  }

}
