package playground.calculator

import cats.free.Free
import cats.{Id, ~>}

class Calculator1 {
  def add(a: Int, b: Int): Int = a + b
  def mult(a: Int, b: Int): Int = a * b
}

object CalculatorDeepEmbeding {

  trait Expression extends Product with Serializable {
    def run[A](calculator2: Calculator2[A]) = calculator2.run(this)
  }
  case class Lit(a: Int) extends Expression
  case class Add(a: Expression, b:Expression) extends Expression
  case class Mult(a: Expression, b: Expression) extends Expression
  case class Subtract(a: Expression, b: Expression) extends Expression

  trait Calculator2[R] {
    def run(e: Expression): R
  }

  class EvalCalc extends Calculator2[Int] {
    def run(e: Expression): Int = e match {
      case Lit(a) => a
      case Add(a, b) => run(a) + run(b)
      case Mult(a, b) => run(a) * run(b)
    }
  }


  class PrinterCalc extends Calculator2[String] {
    def run(e: Expression): String = e match {
      case Lit(a) => a.toString
      case Add(a, b) => "(" + run(a) + " + " + run(b) + ")"
      case Mult(a, b) => "(" + run(a) + " * " + run(b) + ")"
    }
  }

  val eval = new EvalCalc
  val print = new PrinterCalc
  val evalSub = new EvalCalcWithSub

  class EvalCalcWithSub extends EvalCalc {
    override def run(e: Expression): Int = e match {
      case Subtract(a, b) => super.run(a) - super.run(b)
      case e => super.run(e)
    }
  }
}

object FreeCalc {
  trait Expression2[A] extends Product with Serializable
  case class Lit[A](a: Int) extends Expression2[A]
  case class Add[A](a: Expression2[A], b: Expression2[A]) extends Expression2[A]
  case class Mult[A](a: Expression2[A], b: Expression2[A]) extends Expression2[A]


  type Exp[B] = Free[Expression2, B]

  def lit[A](a: Int): Expression2[A] = Lit(a)

  def add[A](a: Expression2[A], b: Expression2[A]): Expression2[A] = Add(a, b)

  def mult[A](a: Expression2[A], b: Expression2[A]): Expression2[A] = Mult(a, b)

  def eval: Expression2 ~> Id = new (Expression2 ~> Id) {
    override def apply[A](fa: Expression2[A]): Id[A] = eval(fa).asInstanceOf[Id[A]]

    def eval[A](expression2: Expression2[A]): Int = expression2 match {
      case Lit(n) => n
      case Add(a, b) => eval(a) + eval(b)
      case Mult(a, b) => eval(a) * eval(b)
    }
  }

  def print: Expression2 ~> Id = new (Expression2 ~> Id) {
    override def apply[A](fa: Expression2[A]): Id[A] = eval(fa).asInstanceOf[Id[A]]

    def eval[A](expression2: Expression2[A]): String = expression2 match {
      case Lit(n) => n.toString
      case Add(a, b) => "(" + eval(a) + " + " + eval(b) + ")"
      case Mult(a, b) => "(" + eval(a) + " * " + eval(b) + ")"
    }
  }
}


object FreeCalc2 {

  trait Expression2[A] extends Product with Serializable
  case class Lit[A](a: Int) extends Expression2[A]
  case class Add[A](a: A, b: A) extends Expression2[A]
  case class Mult[A](a: A, b: A) extends Expression2[A]

  type ExprAlg[B] = Free[Expression2, B]

  def lit[A](a: Int): ExprAlg[A] = Free.liftF(Lit(a))

  def add[A](a: A, b: A): ExprAlg[A] = Free.liftF(Add(a, b))

  def mult[A](a: A, b: A): ExprAlg[A] = Free.liftF(Mult(a, b))

  def eval: Expression2 ~> Id = new (Expression2 ~> Id) {
    override def apply[A](fa: Expression2[A]): Id[A] = eval(fa).asInstanceOf[A]

    def eval[A](expression2: Expression2[A]): Int = expression2 match {
      case Lit(n) => n
      case Add(a, b) => a.asInstanceOf[Int] + b.asInstanceOf[Int]
      case Mult(a, b) => a.asInstanceOf[Int] * b.asInstanceOf[Int]
    }
  }

  def print: Expression2 ~> Id = new (Expression2 ~> Id) {
      override def apply[A](fa: Expression2[A]): Id[A] = eval(fa).asInstanceOf[A]

      def eval[A](expression2: Expression2[A]): String = expression2 match {
        case Lit(n) => n.toString
        case Add(a, b) => "(" + a.toString + " + " + b.toString + ")"
        case Mult(a, b) => "(" + a.toString + " * " + b.toString + ")"
      }
    }


}


object TaglessCalc {
  import cats.Id

  trait TagCalc[F[_], A] {
    def lit(a: Int): F[A]
    def add(a: F[A], b: F[A]): F[A]
    def mult(a: F[A], b: F[A]): F[A]
  }

  val eval =  new TagCalc[Id, Int] {
    override def lit(a: Int): Id[Int] = a
    override def add(a: Int, b: Int): Id[Int] = a + b

    override def mult(a: Int, b: Int): Id[Int] = a * b

  }

  val evalOpt =  new TagCalc[Option, Int] {
    override def lit(a: Int): Option[Int] = Option(a)
    override def add(a: Option[Int], b: Option[Int]): Option[Int] = for {
      x <- a
      y <- b
    } yield x + y

    override def mult(a: Option[Int], b: Option[Int]): Option[Int] = for {
      x <- a
      y <- b
    } yield x * y

  }

  val print = new TagCalc[Id, String] {

    override def lit(a: Int): Id[String] = a.toString

    override def add(a: Id[String], b: Id[String]): Id[String] = s"($a + $b)"

    override def mult(a: Id[String], b: Id[String]): Id[String] = s"($a * $b)"
  }


}
