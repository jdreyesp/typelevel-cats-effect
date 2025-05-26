package com.rockthejvm.part2effects

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import scala.util.{Try, Success, Failure}

object IOErrorHandling extends IOApp {

  // IO: We know already: pure, delay, defer
  // we now want to create failed effect
  val aFailedCompute: IO[Int] =
    IO.delay(throw new RuntimeException("A FAILURE"))

  val aFailure: IO[Int] = IO.raiseError(
    new RuntimeException("a proper fail")
  ) // better notation than IO.delay

  // handle exceptions
  val dealWithIt = aFailure.handleErrorWith { case _: RuntimeException =>
    IO.delay(println("I'm still here"))
  // add more cases
  }

  // turn into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and the success in one go
  val resultAsString =
    aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")

  // redeemWith
  val resultAsEffect = aFailure.redeemWith(
    ex => IO(println(s"FAIL: $ex")),
    value => IO(println(s"SUCCESS: $value"))
  )

  /*
    Exercises
   */
  // 1 - construct potentially failed IO from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match {
      case Some(value) => IO.pure(value)
      case None        => IO.raiseError(ifEmpty)
    }

  def try2IO[A](`try`: Try[A]): IO[A] = `try` match {
    case Success(value) => IO.pure(value)
    case Failure(ex)    => IO.raiseError(ex)
  }

  def either2IO[A](either: Either[Throwable, A]): IO[A] = either match {
    case Right(value) => IO.pure(value)
    case Left(ex)     => IO.raiseError(ex)
  }

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.handleError(handler)
  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.handleErrorWith(handler)

  override def run(args: List[String]): IO[ExitCode] =
    dealWithIt.as(ExitCode.Success)
}
