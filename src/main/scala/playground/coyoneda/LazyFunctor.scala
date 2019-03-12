package playground.coyoneda

import cats.Functor

trait LazyFunctor[F[_], A] { self =>

  def apply[B](f: A => B): F[B]

  def map[B](f: A => B): LazyFunctor[F, B] = new LazyFunctor[F, B] {
    override def apply[C](g: B => C): F[C] = self.apply(g compose f)
  }

  def run = apply(identity)

}

object LazyFunctor {

  def apply[F[_]: Functor, A](a: F[A]): LazyFunctor[F, A] = new LazyFunctor[F, A]() {

    override def apply[B](f: A => B): F[B] = Functor[F].map(a)(f)
  }

}
