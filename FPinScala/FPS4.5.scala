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

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.forall((op: Option[A]) => op match {
      case Some(a) => true
      case None => false
    }))
      Some(a.map(op => op match {
        case Some(value) => value
        case _ => throw new IllegalStateException()
      }))
    else 
      None
  }

  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = {
    val tmp: List[B] = l.flatMap((a: A) => {
        f(a) match {
          case Some(b) => Seq(b)
          case None => Seq()
        } 
      })
    if (tmp.size == l.size) 
      Some(tmp)
    else 
      None
  }
    
}