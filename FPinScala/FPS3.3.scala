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
}
