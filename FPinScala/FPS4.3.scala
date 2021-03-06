class Option[+A] {
  
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = 
    if(map(f) getOrElse false) this else None

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def of[A](a: A): Option[A] = Some(a)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
    case None => None
    case Some(a) => b match {
      case None => None
      case Some(b) => Some(f(a, b))
    }
  }
}