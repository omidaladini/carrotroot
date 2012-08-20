package carrotroot;

object FailedCompileTimeTry {
  abstract class Header
  case class Header1[A](t: Tuple1[A]) extends Header
  case class Header2[A,B](t: Tuple2[A,B]) extends Header
  case class Header3[A,B,C](t: Tuple3[A,B,C]) extends Header

  case class Relation[A <: Header](tuples: Set[A])

  def select[B <: Header, A <: Header <% B](rel: Relation[A]) : Relation[B] = {
    val conversion = implicitly[A => B]
    Relation[B]( rel.tuples.map (x => conversion(x)) )
  }
}

