package playground.coyoneda

import cats.Functor

trait EventuallyFunctor[F[_], A] { self =>

  type Pivot
  val underlyingValue: F[Pivot]
  val allFunctionsComposed: List[Any => Any] = List()


  def map[B](f: A => B): EventuallyFunctor[F, B] = new EventuallyFunctor[F, B] {

    override type Pivot = self.Pivot
    override val underlyingValue: F[Pivot] = self.underlyingValue
    override val allFunctionsComposed: List[Any => Any] = f.asInstanceOf[Any => Any] :: self.allFunctionsComposed
  }

  def run[B](implicit func: Functor[F]): F[B] = func.map(underlyingValue) { a =>
    val chain = Function.chain(allFunctionsComposed.reverse).asInstanceOf[self.Pivot => B]
    chain(a)
  }

}



object EventuallyFunctor {

  def apply[F[_], A, B](fa: F[A])(f: A => B): EventuallyFunctor[F, A] = new EventuallyFunctor[F, A] {

    override type Pivot = A
    override val underlyingValue: F[A] = fa
    override val allFunctionsComposed: List[Any => Any] = List(f.asInstanceOf[Any => Any])

  }

}
