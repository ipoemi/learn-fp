package learnfp.applicative

import learnfp.functor.Maybe.Maybe
import learnfp.functor.{MaybeInstance => MaybeFunctorInstance}

import learnfp.functor.Maybe.{Maybe, Just, Nothing}


object MaybeInstance {

  import MaybeFunctorInstance._
  import learnfp.functor.FunctorOps._

  implicit val idApplicativeInstance = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] = Just(a)

    override def <*>[A, R](mfx: Maybe[A => R])(ma: Maybe[A]): Maybe[R] = (mfx, ma) match {
      case (Just(fx), Just(a)) => Just(fx(a))
      case (_, _) => Nothing()
    }
  }
}
