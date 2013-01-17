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

  def addOne[F[_] : Functor](f: F[Int]): F[Int] =
    implicitly[Functor[F]].map(f, {a: Int => a + 1})

  trait Monoid[A] {
    def zero: A
    def plus(lhs: A, rhs: A): A
  }

  implicit val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
    def zero: Int = 0
    def plus(lhs: Int, rhs: Int): Int = lhs + rhs
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def zero: String = ""
    def plus(lhs: String, rhs: String): String = lhs + rhs
  }

  implicit def functionMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def zero: A => A = identity
    def plus(lhs: A => A, rhs: A => A): A => A = lhs andThen rhs
  }

  def sum[A : Monoid](l: List[A]): A = {
    val monoid = implicitly[Monoid[A]]
    l.foldLeft(monoid.zero)(monoid.plus)
  }

}
