package playground.coyoneda

import cats.Functor
import cats.free.{Coyoneda, Yoneda}
import org.scalatest.{FeatureSpec, Matchers}

class CoyonedaTest extends FeatureSpec with Matchers {

  scenario("Functors, Yoneda, Coyoneda") {
    val li = List(1, 2, 3)


    implicit val liFunctor = new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)
    }

    val plusOne: List[Int] = Functor[List].map(li)(_ + 1)
    println("plusOne: " + plusOne)
    val timesTen: List[Int] = Functor[List].map(plusOne)(_ * 10)
    println("timesTen: " + timesTen)

    println("-----------------------------------")


    //lazy functor
    val yoneda: Yoneda[List, Int] = Yoneda.apply(li)(liFunctor)

    val yPlusOne: Yoneda[List, Int] = yoneda.map(_ + 1)
    println("yPlusOne: " + yPlusOne)
    val yTimesTen: Yoneda[List, Int] = yPlusOne.map(_ * 10)
    println("yTimesTen: " + yTimesTen)
    println("y run: " + yTimesTen.run)

    println("-----------------------------------")

    //eventual functor
    val coyonedaPlusOne: Coyoneda[List, Int] = Coyoneda.apply(li)(_ + 1)
    println(coyonedaPlusOne)
    val coyonedaTimesTen: Coyoneda[List, Int] = coyonedaPlusOne.map(_ * 10)
    println(coyonedaTimesTen)

    println(coyonedaTimesTen.run(liFunctor))

    println("-----------------------------------")


    println(coyonedaTimesTen.k(9.asInstanceOf[coyonedaTimesTen.Pivot]))

    println("---------------lazy functor--------------------")


    //lazy functor
    val lazyFunctor: LazyFunctor[List, Int] = LazyFunctor.apply(li)(liFunctor)

    val yzPlusOne: LazyFunctor[List, Int] = lazyFunctor.map(_ + 1)
    println("yPlusOne: " + yzPlusOne)
    val yzTimesTen: LazyFunctor[List, Int] = yzPlusOne.map(_ * 10)
    println("yTimesTen: " + yzTimesTen)
    println("y run: " + yzTimesTen.run)

    println("----------------eventual functor-------------------")


    val ecoyonedaPlusOne: EventuallyFunctor[List, Int] = EventuallyFunctor.apply(li)(_ + 1)
    println(ecoyonedaPlusOne)
    val ecoyonedaTimesTen: EventuallyFunctor[List, Int] = ecoyonedaPlusOne.map(_ * 10)
    println(ecoyonedaTimesTen)

    println(ecoyonedaTimesTen.run(liFunctor))

    println("-----------------------------------")




  }

}
