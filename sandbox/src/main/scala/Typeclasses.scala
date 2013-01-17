object Typeclasses {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A], fn: A => B): F[B]
  }

  implicit val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A], fn: A => B): List[B] = fa.map(fn)
  }

  implicit val optionFunctor = new Functor[Option] {
    def map[A, B](fa: Option[A], fn: A => B): Option[B] = fa.map(fn)
  }

  def addOne[F[_]](f: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(f, {a: Int => a + 1})

  trait Monoid[A] {
    def zero: A
    def plus(lhs: A, rhs: A): A
  }

  val intAdditionMonoid: Monoid[Int] = ???
}
