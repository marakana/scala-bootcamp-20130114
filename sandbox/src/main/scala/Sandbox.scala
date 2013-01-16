object Sandbox {

  // data transformation
  def map[A, B](l: List[A], fn: A => B): List[B] = l match {
    case head :: tail => fn(head) :: map(tail, fn)
    case Nil => Nil
  }

  // data filtering
  def filter[A](l: List[A], pred: A => Boolean): List[A] = l match {
    case head :: tail =>
      val rest = filter(tail, pred)
      if (pred(head)) head :: rest else rest
    case Nil => Nil
  }

  // data aggregation
  def foldLeft[A, B](seed: B, l: List[A], fn: (B, A) => B): B = l match {
    case head :: tail => foldLeft(fn(seed, head), tail, fn)
    case Nil => seed
  }

  def sum(l: List[Int]): Int =
    foldLeft(0, l, { (x: Int, y: Int) => x + y })
}