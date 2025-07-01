package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.duration._
import cats.effect.kernel.Outcome
import cats.effect.kernel.Fiber
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

object RacingIOs extends IOApp.Simple {

  import com.rockthejvm.utils.debugIO

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (IO(s"Starting computation").debugIO
      >> IO.sleep(duration)
      >> IO(s"comptuation: done")
      >> IO(value))
      .onCancel(IO(s"computation CANCELED for $value").debugIO.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)

    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    // both IOs run on separate fibers
    // - the first one to finish will complete the result
    // - the loser will be canceled

    first.flatMap {
      case Left(mol)   => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.second)

    // Much more control on what happens
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]),
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) =>
        fibLang.cancel >> IO("MOL won").debugIO >> IO(outMol).debugIO
      case Right((fibMol, outLang)) =>
        fibMol.cancel >> IO("Language won").debugIO >> IO(outLang).debugIO
    }
  }

  /** Exercises:
    *
    * 1 - implement a timeout pattern with race 2 - a method to return a LOSING
    * effect from a race (hint: use racePair) 3 - implement race in terms of
    * racePair
    */
  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeout = IO.sleep(duration)

    IO.race(io, timeout).flatMap {
      case Left(value)  => IO(s"Finished successfully").debugIO >> io
      case Right(value) => IO.raiseError(new Exception("Timeout"))
    }
  }

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fiberb)) =>
        fiberb.join.flatMap {
          case Succeeded(resultEffect) =>
            resultEffect.map(result => Right(result))
          case Errored(e) => IO.raiseError(e)
          case Canceled() =>
            IO.raiseError(new RuntimeException("Loser canceled."))
        }
      case Right((fibera, _)) =>
        fibera.join.flatMap {
          case Succeeded(resultEffect) =>
            resultEffect.map(result => Left(result))
          case Errored(e) => IO.raiseError(e)
          case Canceled() =>
            IO.raiseError(new RuntimeException("Loser canceled."))
        }
    }
  }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) =>
        outA match {
          case Succeeded(valueA) => fibB.cancel >> valueA.map(a => Left(a))
          case Errored(e)        => fibB.cancel >> IO.raiseError(e)
          // the winner is not a real winner
          case Canceled() =>
            fibB.join.flatMap {
              case Succeeded(effectB) => effectB.map(b => Right(b))
              case Errored(e)         => IO.raiseError(e)
              case Canceled() =>
                IO.raiseError(
                  new RuntimeException("Both computations canceled.")
                )
            }
        }
      case Right((fibA, outB)) =>
        outB match {
          case Succeeded(valueB) => fibA.cancel >> valueB.map(b => Right(b))
          case Errored(e)        => fibA.cancel >> IO.raiseError(e)
          // the winner is not a real winner
          case Canceled() =>
            fibA.join.flatMap {
              case Succeeded(effectA) => effectA.map(a => Left(a))
              case Errored(e)         => IO.raiseError(e)
              case Canceled() =>
                IO.raiseError(
                  new RuntimeException("Both computations canceled.")
                )
            }
        }
    }

  override def run: IO[Unit] = {
    val ioa = IO.sleep(1.second) >> IO(s"FibA")
    val iob = IO.sleep(2.seconds) >> IO(s"FibB")
    simpleRace(ioa, iob).debugIO.void
  }

  // unrace(IO.sleep(1.second), IO.sleep(2.seconds)).debugIO.void

  // timeout(IO.sleep(0.5.second), 1.second).debugIO
  // testRacePair().debugIO.void
  // testRace().debugIO.void
}
