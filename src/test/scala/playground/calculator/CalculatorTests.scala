package playground.calculator

import cats.free.Free
import org.scalatest.{FeatureSpec, Matchers}
import playground.calculator.TaglessCalc.TagCalc

class CalculatorTests extends FeatureSpec with Matchers {

  scenario("calculator 1") {
    val c = new Calculator1()

    val r = c.mult(
      2,
      c.add(4, 6)
    )

    r shouldBe 20

  }

  scenario("calculator 2") {

    import CalculatorDeepEmbeding._

    val program: Expression =  Mult(Lit(2), Add(Lit(4), Lit(6)))

    program.run(eval) shouldBe 20
    program.run(print) shouldBe "(2 * (4 + 6))"

    val program2: Expression = Subtract(
      Mult(Lit(2), Add(Lit(4), Lit(6))),
      Lit(1)
    )

    program2.run(evalSub) shouldBe 19
  }



  scenario("Free Monad") {
    import FreeCalc._

    val program: Expression2[Int] = mult(lit(2), add(lit(4), lit(6)))

    val liftedProgram = Free.liftF(program)

    liftedProgram.foldMap(eval) shouldBe 20
    liftedProgram.foldMap(print) shouldBe "(2 * (4 + 6))"
  }

  scenario("Free Monad ----= 2 =----") {
    import FreeCalc2._

    val program = for {
      two <- lit[Int](2)
      four <- lit[Int](4)
      sum <- add[Int](two, four)
    } yield sum

    program.foldMap(eval) shouldBe 6
    program.foldMap(print) shouldBe "(2 + 4)"
  }

  scenario("Tagless Final") {
    def program[F[_], A](c: TagCalc[F, A]): F[A] = c.mult(c.lit(2), c.add(c.lit(4), c.lit(6)))

    program(TaglessCalc.eval) shouldBe 20
    program(TaglessCalc.evalOpt) shouldBe Option(20)
    program(TaglessCalc.print) shouldBe "(2 * (4 + 6))"
  }

}
