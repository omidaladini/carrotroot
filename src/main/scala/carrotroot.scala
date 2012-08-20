package carrotroot

object CarrotRoot {

 case class M[A](value: A) {
    def bind[B](k: A => M[B]): M[B] =  k(value)
    def map[B](f: A => B): M[B] =  bind(x => unitM(f(x)))
    def flatMap[B](f: A => M[B]): M[B] = bind(f)
  }

  def unitM[A](a: A): M[A] = M(a)

  import scala.collection.immutable.Set

  type Tuple = Set[Symbol]

  case class Relation(tuples: Set[Tuple])

  trait Algebra
  case class Projection(rel: Relation, on: Tuple) extends Algebra
  case class Union(rel1: Relation, rel2: Relation) extends Algebra
  case class Difference(rel1: Relation, rel2: Relation) extends Algebra

  def interp(alg: Algebra): M[Relation] = alg match {
    case Projection(rel1, rel2) => for (val a <- interp(l, e);
         val b <- interp(r, e);
         val c <- add(a, b))
                      yield c
    case Lam(x, t) => unitM(Fun(a => interp(t, Pair(x, a) :: e)))
    case App(f, t) => for (val a <- interp(f, e);
         val b <- interp(t, e);
         val c <- apply(a, b))
          yield c
  }



  case class Projection(rel: Relation, on: Tuple) extends Transformation {
    def map[B](f: A => B): Transformation[B]
  }

  def restrict(tuple: Tuple, on: Tuple) : Tuple
  = tuple -- on

  def project(rel: Relation, on: Tuple) : Relation
  = Relation(rel.tuples.map(restrict(_, on)))
*/
}
