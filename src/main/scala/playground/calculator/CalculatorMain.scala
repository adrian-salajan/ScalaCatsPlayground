package playground.calculator

import scala.io.StdIn

object CalculatorMain {

  def main(args: Array[String]): Unit = {

    import CalculatorDeepEmbeding._

    println("test read int = ")
    StdIn.readInt()

    val two: Expression = ReadInt
    val exp1: Expression = Add(two, two)
    println(exp1.run(eval))
    println("------------")
    val exp2: Expression = Add(ReadInt, ReadInt)
    println(exp2.run(eval))
  }


}
