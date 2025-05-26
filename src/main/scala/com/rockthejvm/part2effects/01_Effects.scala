package com.rockthejvm.part2effects

import scala.concurrent.Future

object Effects extends App {

  // functional programming = functions as expressions that are first-class citizens of our applications
  // pure functional programming = a single expression that can be composed of many other ones inside (with no side effects)

  // substitution = I can replace an expression with the value that it evaluates to
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // referential transparency = replace an expression with its value as many times as we want without changing behavior

  // not referential transparent examples
  object NonRTExamples {
    // e.g. print to console
    val printSomething: Unit = println("Cats effect")
    val printSomething_v2: Unit = ()
    // these two are not the same! Referential transparency has been broken

    // e.g. changing a variable
    var anInt = 0
    val changingVar: Unit = (anInt += 1)
    val changingVar_v2: Unit = () // not the same!
  }

  object Effects {
    // Side effects are inevitable for useful programs

    // effect - data type that embodies the concept of side effect
    /*
    Desires:
      - 1. type signature describes the kind of calculation that will be performed
      - 2. type signature describes the VALUE that will be calculated
      - 3. when side effects are needed, effect construction is separate from effect execution
     */

    // example: Option
    // applies 1 = possibly absent value √
    // applies 2 = It wraps an Int, so we know the value that will be calculated, if it exists √
    // applies 3 = side effects are not needed, since the construction of the option simply wraps a value √
    // Conclusion: Option IS an effect type
    val anOption: Option[Int] = Option(42)

    // example: Future
    // applies 1 = describes an asynchronous computation
    // applies 2 = computes a value of type A, if it's successful
    // DO NOT apply 3 = Side effect ARE needed to run a future: Allocating / scheduling a thread => Is it separate from its construction?? No. At the moment we do `Future(42)`
    // it also runs side effects.
    // Conclusion: Future IS NOT an effect type
    import scala.concurrent.ExecutionContext.Implicits.global
    val aFuture: Future[Int] = Future(42)
  }

  // example: MyIO data type from the Monads lesson (in Rocksthejvm cats course)
  // applies 1 = describes any computation that might produce side effects
  // applies 2 = calculates a value of type A, if it's succcessful
  // applies 3 = side effects ARE required for the evaluation of () => A => Is it separate from its construction?? Yes! The creation of MyIO does NOT produce the side effect on construction
  // Conclusion: MyIO is an effect type (in fact it's the more generic one we can define)
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

}
