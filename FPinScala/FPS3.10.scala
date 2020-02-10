sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def removeHead[A] (list: MyList[A]): MyList[A] = list match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A] (newHead: A, list: MyList[A]): MyList[A] = 
    Cons(newHead, list): MyList[A]

  def drop[A] (l: MyList[A], n: Int): MyList[A] = n match {
    case i if (i <= 0 || l == Nil) => l
    case _ => drop(removeHead(l), n - 1)
  }

  def dropWhile[A] (l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case Nil => Nil
    case Cons(x, t) if (f(x)) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case Nil => l
    case Cons(_, Nil) => l
    case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
    case Cons(x, y) => Cons(x, init(y))
  }

  def foldRight[A,B](l: MyList[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: MyList[A]): Int =
    foldRight(l, 0)((x,y) => y + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: MyList[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, t) => foldLeft(t, f(z, x))(f)
  }
}
