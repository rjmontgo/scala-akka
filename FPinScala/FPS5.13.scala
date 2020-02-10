sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(head, tail) => head() :: tail().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => if (n == 1) Stream.cons(head(), Empty) else Stream.cons(head(), tail().take(n - 1))
  }

  def takeUnfold(n: Int): Stream[A] = 
    Stream.unfold((this, n))(s => 
      if (s._2 == 0)
        None 
      else 
        s._1 match {
          case Cons(head, tail) => Some((head(), (tail(), s._2 -1)))
          case Empty => None
        }
      )

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => if (n == 1) tail() else tail().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => if (p(head())) Stream.cons(head(), tail().takeWhile(p)) else Empty
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = 
    Stream.unfold((this, p))(s => s._1 match {
      case Empty => None
      case Cons(head, tail) => 
        if (s._2(head())) 
          Some(head(), (tail(), p))
        else 
          None
    })

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

  def mapUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s match {
      case Cons(head, tail) => Some(f(head()), tail())
      case Empty => None
    })
    

  def filter(f: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) => if (f(head())) Stream.cons(head(), tail().filter(f)) else tail().filter(f)
    case Empty => Empty
  }

  def append[B >: A](a: => B): Stream[B] = this match {
    case Cons(head, tail) => Stream.cons(head(), tail().append(a))
    case Empty => Stream.cons(a, Empty)
  }

  def zipWith[B >: A](stream: Stream[B], f: (A, B) => B): Stream[B] = 
    Stream.unfold((this, stream))(s => s._1 match {
      case Empty => None
      case Cons(head, tail) => s._2 match {
        case Empty => None
        case Cons(head2, tail2) => Some((f(head(), head2()), (tail(), tail2())))
      }
    })
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
    Stream.cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  def fibs: Stream[Int] = {
    def fib(prev: Int, current: Int): Stream[Int] = {
      Stream.cons(current, fib(current, prev + current))
    }
    Stream.cons(0, fib(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case Some(value: (a, s)) => Stream.cons(value._1, unfold(value._2: S)(f))
      case None => Empty
    }

  def fibUnfold(): Stream[Int] = 
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s+1))

  def constant(n: Int): Stream[Int] = 
    unfold(n)(s => Some(s, s))

  def ones: Stream[Int] = 
    constant(1)
}