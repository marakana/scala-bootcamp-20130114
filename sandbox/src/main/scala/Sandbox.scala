import annotation.tailrec

object Sandbox {

  // data transformation
  def map[A, B](l: List[A])(fn: A => B): List[B] = {
    val backwards = foldLeft(List.empty[B], l) {
      (acc: List[B], elem: A) => fn(elem) :: acc
    }
    backwards.reverse
  }

  // data filtering
  def filter[A](l: List[A])(pred: A => Boolean): List[A] = {
    val backwards = foldLeft(List.empty[A], l) {
      (acc: List[A], elem: A) =>
        if (pred(elem)) elem :: acc else acc
    }
    backwards.reverse
  }

  // data aggregation
  @tailrec
  def foldLeft[A, B](seed: B, l: List[A])(fn: (B, A) => B): B = l match {
    case head :: tail => foldLeft(fn(seed, head), tail)(fn)
    case Nil => seed
  }

  def sum(l: List[Int]): Int = {
    @tailrec
    def go(l: List[Int], acc: Int): Int = l match {
      case head :: tail =>
        val newAcc = head + acc
        go(tail, newAcc)
      case Nil => acc
    }
    go(l, 0)
  }

}