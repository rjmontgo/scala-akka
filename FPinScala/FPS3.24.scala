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

  def reverse[A](l: MyList[A]): MyList[A] =
    foldLeft(l, Nil:MyList[A])((prev, current) => Cons(current, prev))

  def append[A](l: MyList[A], z: A): MyList[A] =
    foldRight(l, Cons(z, Nil))(Cons(_,_))

  def add1[A](l: MyList[Int]): MyList[Int] = l match {
    case Nil => l
    case Cons(x, xs) => Cons(x + 1, add1(xs))
  }

  def DoubletoString[A](l: MyList[Double]): MyList[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString(), DoubletoString(xs))
  }

  def map[A, B](l: MyList[A])(f: (A => B)): MyList[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](l: MyList[A])(f: (A => Boolean)): MyList[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }
  
  def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] = { 
    def appendLast(list1: MyList[B], list2: MyList[B]): MyList[B] = list1 match {
      case Nil => list2
      case Cons(x, Nil) => Cons(x, list2)
      case Cons(_, y) => appendLast(y, list2)
    }
    l match {
      case Nil => Nil
      case Cons(x, xs) => appendLast(f(x), flatMap(xs)(f))
    }
  }

  def addIntList(l: MyList[Int], m: MyList[Int]): MyList[Int] = l match {
    case Nil => Nil
    case Cons(x, y) => m match {
      case Cons(mx, my) => Cons(mx + x, addIntList(y, my))
    }
  }

  def zipWith[A](l: MyList[A], m: MyList[A])(f: (A, A) => A): MyList[A] = l match {
    case Nil => Nil
    case Cons(x, y) => m match {
      case Cons(mx, my) => Cons(f(x, mx), zipWith(y, my)(f))
    }
  }

  def flatMapFilter[A](l: MyList[A])(f: (A => Boolean)): MyList[A] = 
    flatMap(l)((a: A) => if (f(a)) Cons(a, Nil) else Nil)

  def hasSubsequence[A](l: MyList[A], m: MyList[A]): Boolean = {
    // filter where l == m
    // partition list
    def matchVal(y: MyList[A], my: MyList[A]): Boolean = {
      y match {
        case Nil => my == Nil
        case Cons(heady, taily) => my match {
          case Nil => true
          case Cons(headmy, tailmy) => headmy == heady && matchVal(taily, tailmy)
        }
      }
    }
    l match {
      case Nil => false
      case Cons(x, y) => m match {
        case Nil => false
        case Cons(mx, my) => if ( x == mx && matchVal(y, my)) true else hasSubsequence(y, my)
      }
    }
  }
} 
