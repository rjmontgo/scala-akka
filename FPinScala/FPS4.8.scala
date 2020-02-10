sealed abstract class Either[+E, +A] {

  // all these functions operate on the Right value
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(e) => Left(e)
    case Right(a) => b match {
      case Left(e) => Left(e)
      case Right(bval) => Right(f(a, bval))
    }
  }

}

final case class Left[+E](error: E) extends Either[E, Nothing]
final case class Right[+A](value: A) extends Either[Nothing, A]


object MyEither {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = 
    es match {
      case Nil => Right(Nil)
      case head :: tail => head match {
        case Left(e) => Left(e)
        case Right(value) => 
          sequence(tail) match {
            case Left(e) => Left(e)
            case Right(retVal) => Right(value :: retVal)
          }
      }
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    MyEither.sequence(as map f)
  
}