sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(head, tail) => head() :: tail().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => if (n == 1) Stream.cons(head(), Empty) else Stream.cons(head(), tail().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => if (n == 1) tail() else tail().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => if (p(head())) Stream.cons(head(), tail().takeWhile(p)) else Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(head, tail) => p(head()) && tail().forAll(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case Empty => z
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = 
    foldRight(Empty: Stream[A])((current, rest) => 
      if (p(current)) 
        Stream.cons(current, rest) 
      else Empty
    )

  def headOption: Option[A] =
    foldRight(None: Option[A])((current, tail) => Some(current)) 

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(head, tail) => Stream.cons(f(head()), tail().map(f))
    case Empty => Empty
  }

  def filter(f: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) => if (f(head())) Stream.cons(head(), tail().filter(f)) else tail().filter(f)
    case Empty => Empty
  }

  def append[B >: A](a: => B): Stream[B] = this match {
    case Cons(head, tail) => Stream.cons(head(), tail().append(a))
    case Empty => Stream.cons(a, Empty)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val ones: Stream[A] = Stream.cons(a, ones)
    ones
  }
}