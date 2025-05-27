package com.rockthejvm.part2effects

import cats.effect.IOApp
import cats.effect.IO
import cats.Parallel

object IOParallelism extends IOApp.Simple {

  // IOs are usually sequential
  val aniIO = IO(s"[${Thread.currentThread().getName()}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName()}] Kamran")

  val composedIO = for {
    ani <- aniIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  // debug with thread information extension method
  import com.rockthejvm.utils._
  import cats.syntax.apply._

  // here is a demonstration of how IOs are sequential - they will use the same JVM thread for its execution
  val meaningOfLife = IO.delay(42).debugIO
  val favLang = IO.delay("Scala").debugIO
  val goalInLife = (meaningOfLife, favLang).mapN((num, string) =>
    s"my goal in life is $num and $string"
  )

  // parallelism
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang)
  val goalInLifeParallel: IO.Par[String] =
    (parIO1, parIO2).mapN((num, string) =>
      s"my goal in life is $num and $string"
    )

  // back to sequential
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand: parMapN
  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] =
    (meaningOfLife, favLang).parMapN((num, string) =>
      s"my goal in life is $num and $string"
    )

  // what happens if one of our IO fail?
  val aFailure: IO[String] =
    IO.raiseError(new RuntimeException("I can't do this"))

  // compose success + failure => it fails if one of the IOs fail
  val parallelWithFailure =
    (meaningOfLife.debugIO, aFailure.debugIO).parMapN(_ + _)

  // compose failure + failure => it fails
  val anotherFailure: IO[String] =
    IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] =
    (aFailure.debugIO, anotherFailure.debugIO).parMapN(_ + _)

  // the first effect to fail gives the failure of the result!
  val twoFailuresFirstDelayed: IO[String] =
    (IO(Thread.sleep(1000)) >> aFailure.debugIO, anotherFailure.debugIO)
      .parMapN(
        _ + _
      ) // this returns the second error since it was the first one to fail
  override def run: IO[Unit] =
    // parallelWithFailure.void
    // twoFailures.void
    twoFailuresFirstDelayed.void
}
