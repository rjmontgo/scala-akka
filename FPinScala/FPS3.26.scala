sealed trait Tree[+A]
case object Nil extends Tree[Nothing]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Nil => 0
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Nil => 0
    case Leaf(x) => x
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

}
