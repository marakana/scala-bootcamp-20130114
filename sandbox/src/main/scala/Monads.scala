object Monads {

  /*
   * Monads represent sequencing/composing operations through
   * some computational context. The monad encapsulates the context
   * and allows the programmer to focus on the sequence of operations.
   */

  // Option represents the context of possible failures
  sealed trait Option[+A] {
    def map[B](fn: A => B): Option[B]
    def flatMap[B](fn: A => Option[B]): Option[B]
  }

  object Option {

    case class Some[+A](value: A) extends Option[A] {
      def map[B](fn: A => B): Option[B] = Some(fn(value))
      def flatMap[B](fn: A => Option[B]): Option[B] = fn(value)
    }

    case object None extends Option[Nothing] {
      def map[B](fn: Nothing => B): Option[B] = None
      def flatMap[B](fn: Nothing => Option[B]): Option[B] = None
    }

  }

}
