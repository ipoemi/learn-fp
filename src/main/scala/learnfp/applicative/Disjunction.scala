package learnfp.applicative

import learnfp.functor.Disjunction._
import learnfp.functor.DisjunctionInstance._

object DisjunctionInstance {
  implicit def disjunctionInstance[L] = new Applicative[({type E[X] = Disjunction[L, X]})#E]() {
    override def pure[A](a: A): Disjunction[L, A] = right(a)

    override def <*>[A, R](dfx: Disjunction[L, A => R])(da: Disjunction[L, A]): Disjunction[L, R] = (dfx, da) match {
      case (RightDisjunction(fx), RightDisjunction(a)) => right(fx(a))
      case (LeftDisjunction(l1), _) => left(l1)
      case (_, LeftDisjunction(l1)) => left(l1)
    }
  }

  implicit def disjunctionToApplicativeOps[L, A, R](fx: Disjunction[L, A => R])(
    implicit applicative: Applicative[({type E[X] = Disjunction[L, X]})#E]) =
    new FxApplicativeOps[A, R, ({type E[X] = Disjunction[L, X]})#E](fx)
}
