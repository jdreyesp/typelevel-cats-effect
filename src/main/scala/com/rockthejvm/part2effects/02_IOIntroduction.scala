package com.rockthejvm.part2effects

import cats.effect.IO
import scala.io.StdIn
import cats.effect.IOApp
import cats.effect.ExitCode

object IOIntroduction extends IOApp {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(
    42
  ) // arg that should not have side effects! Don't do this if your computation may execute side effects

  val aDelayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    54
  }

  val aDelayedIO_v2: IO[Int] = IO { // apply == delay
    println("I'm producing an integer")
    54
  }

  // map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN (from Apply type class) to combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] =
    (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /*
    Exercises
   */
  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    r1 <- ioa
    r2 <- iob
  } yield r2

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // *> == andThen

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // >> == andThen (by name)

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    for {
      r1 <- ioa
      r2 <- iob
    } yield r1

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa <* iob

  // 3 - Repeat an IO effect forever
  // hint: use flatMap + recursion
  def forever[A](io: IO[A]): IO[A] = io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] = io >> forever_v2(io)
  // >> (lazy `andThen`) is recommended over flatMap since under the scenes, since IO is using by name, the implementation will be
  // an infinite Linked list of recursive IO operations, so this implementation prevents StackOverflows

  def forever_v3[A](io: IO[A]): IO[A] = io *> forever_v2(io)

  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM // tail recursive implementation

      // 4 - convert an IO to a different type
      // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] = ioa.as(()) // confusing, don't use

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] = ioa.void // encouraged

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  // returns an IO that has all the numbers sum up to n
  def sumIO(n: Int): IO[Int] = {
    if (n <= 0) IO(0)
    else
      // this for comprehension won't crash with StackOverflow on big numbers since the implementation of IO
      // under the hood is stack safe.
      for {
        lastNumber <- IO(n)
        prevSum <- sumIO(n - 1)
      } yield prevSum + lastNumber

  }

  override def run(args: List[String]): IO[ExitCode] =
    val ios: List[IO[BigInt]] = (1 to 100).toList.map { n =>
      for {
        fib <- IO.defer(
          fibonacci(n)
        ) // IO.defer == IO(fibonacci(n)).flatten => in the end it's deferring its execution until it's needed
        _ <- IO.println(fib)
      } yield fib
    }

    ios.sequence.as(ExitCode.Success)

    // smallProgram_v2().as(ExitCode.Success)

  // 7 (hard) - write a fibonacci IO that does NOT crash on recursion
  // hints: use recursion, ignore exponential complexity, and use flatMap heavily
  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else
      for {
        last <- fibonacci(n - 1) // or .flatMap(x => x)
        prev <- fibonacci(n - 2)
      } yield last + prev

  // other way of running IOs

  // import cats.effect.unsafe.implicits.global // "platform" (thread pool + IO Runtime) used for unsafeRunSync
  // this should be called only at the end of your application
  // println(aDelayedIO.unsafeRunSync())
  // smallProgram_v2().unsafeRunSync()
}
