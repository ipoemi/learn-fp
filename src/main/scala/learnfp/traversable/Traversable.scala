package learnfp.traversable

import learnfp.applicative.Applicative
import learnfp.foldable.Foldable
import learnfp.functor.{Functor, Id}


trait Traversable[C[_]] {
  def traverse[A, B, F[_]](xs: C[F[A]])
    (fx: A => B)
    (implicit foldable: Foldable[C], functor: Functor[F], applicative: Applicative[F]): F[C[B]]
}

object TraversableInstances {

  import learnfp.foldable.FoldableInstances._
  import learnfp.applicative.ApplicativeOps._
  import learnfp.functor.FunctorOps._

  implicit val idTraversableInstance = new Traversable[Id] {
    override def traverse[A, B, F[_]](xs: Id[F[A]])
      (fx: A => B)
      (implicit foldable: Foldable[Id], functor: Functor[F], applicative: Applicative[F]): F[Id[B]] =
      functor.fmap(xs.value)(x => Id(fx(x)))

  }

  type STuple3[A] = (A, A, A)

  def stuple3[A](a: A, b: A, c: A): STuple3[A] = (a, b, c)

  implicit val tuple3TraversableInstance = new Traversable[STuple3] {
    override def traverse[A, B, F[_]](xs: (F[A], F[A], F[A]))(fx: A => B)(implicit foldable: Foldable[STuple3],
      functor: Functor[F], applicative: Applicative[F]): F[(B, B, B)] = {
      import functor.fmap
      import applicative.<*>
      import applicative.pure

      <*>(
        fmap(xs._1)(a1 => (b: (B, B)) => (fx(a1), b._1, b._2))
      )(<*>(
        fmap(xs._2)(a2 => (b: B) => (fx(a2), b))
      )(fmap(xs._3)(fx)))
      /*
      <*>(
        <*>(
          fmap(xs._1)(a1 => (a2: A) => (a3: A) => (fx(a1), fx(a2), fx(a3))))
        (xs._2)
      )(xs._3)
      */
      /*
      val aa = foldable.foldr(xs)(pure(List.empty[B])){ (fa, fbs) =>
        <*>(fmap(fa)(a => (xs: List[B]) => fx(a) :: xs))(fbs)
      }
      fmap(aa)(x => (x(0), x(1), x(2)))
      */
    }
  }

  implicit val listTraversableInstance = new Traversable[List] {
    override def traverse[A, B, F[_]](xs: List[F[A]])(fx: A => B)(implicit foldable: Foldable[List],
      functor: Functor[F], applicative: Applicative[F]): F[List[B]] = {
      import foldable.foldr
      import functor.fmap
      import applicative.<*>
      import applicative.pure

      foldr(xs)(pure(List.empty[B])) { (fa, fxs) =>
        <*>(fmap(fa)(a => (xs: List[B]) => fx(a) :: xs))(fxs)
      }
    }
  }
}

class TraversableOps[A, C[_], F[_]](xs: C[F[A]])(implicit foldable: Foldable[C], traversable: Traversable[C],
  functor: Functor[F], applicative: Applicative[F]) {
  def traverse[B](fx: A => B): F[C[B]] = traversable.traverse(xs)(fx)

  def sequence: F[C[A]] = traverse(identity)
}

object TraversableOps {
  implicit def toTraversableOps[A, C[_], F[_]](xs: C[F[A]])(implicit functor: Functor[F], applicative: Applicative[F],
    foldable: Foldable[C], traversable: Traversable[C]) = new TraversableOps[A, C, F](xs)
}
