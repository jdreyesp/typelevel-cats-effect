package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import com.rockthejvm.part2effects.IOParallelism.favLang
import cats.effect.kernel.Fiber
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Succeeded, Errored, Canceled}
import scala.concurrent.duration._

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLong = IO.pure("Scala")

  import com.rockthejvm.utils._

  def sameThreadIOs() = for {
    _ <- meaningOfLife.debugIO
    _ <- favLang.debugIO
  } yield ()

  // introduce the fiber
  // fiber = lightweight, managed, concurrent thread of execution, on the cats runtime.
  // jvm threads= heavy , fiber = lightweight
  def createFiber: Fiber[IO, Throwable, String] =
    ??? // almost impossible to create fibers manually, since they depend on the cats runtime

  // how to create a fiber
  // this does not start the fiber, but wraps it into an IO (a fiber is not an effect, its construction contains side effects)
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debugIO.start

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLang.debugIO
  } yield ()

  // join a fiber - wait for a fiber to finish
  // the Outcome is explained below
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result

  /* The possible Outcomes are:
    - success with an IO
    - failure with an exception
    - cancelled
   */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(fa) => fa
    case Errored(e)    => IO(0)
    case Canceled()    => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task =
      IO("Starting").debugIO >> IO.sleep(1.second) >> IO("done").debugIO
    val taskWithCancellationHandler =
      task.onCancel(IO("I'm being cancelled!").debugIO.void)

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO(
        "Cancelling"
      ).debugIO // running on main thread
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /** Exercises:
    *
    * \1. Write a function that runs an IO on another thread, and, depending on
    * the result of the fiber \- return the result in an IO \- if errored or
    * cancelled, return a failed IO
    *
    * 2. Write a function that takes two IOs, runs them on different fibers and
    * returns an IO with a tuple containing both results \- if both IOs complete
    * successfully, tuple their results \- if the first IO returns an error,
    * raise that error (ignoring the second IO's result/error) \- if the first
    * IO doesn't error but second IO returns an error, raise that error \- if
    * one (or both) canceled, raise a RuntimeException
    *
    * 3. Write a function that adds a timeout to an IO: \- IO runs on a fiber \-
    * if the timeout duration passes, then the fiber is canceled \- the method
    * returns an IO[A] which contains \- the original value if the computation
    * is successful before the timeout signal \- the exception if the
    * computation is failed before the timeout signal \- a RuntimeException if
    * it times out (i.e. cancelled by the timeout)
    */
  // 1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val outcome = for {
      fib <- io.start
      result <- fib.join
    } yield result

    outcome.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) =>
        IO.raiseError(new RuntimeException(s"Errored result $e"))
      case Canceled() =>
        IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {

    val outcome = for {
      fib1 <- ioa.start
      fib2 <- iob.start
      r1 <- fib1.join
      r2 <- fib2.join
    } yield (r1, r2)

    outcome.flatMap {
      case (Succeeded(fa), Succeeded(fb)) =>
        fa.flatMap(a => fb.map(b => (a, b)))
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(throw new RuntimeException("Cancelled!"))
    }
  }
  // 3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computationResult = for {
      fib <- io.start
      _ <- IO(Thread.sleep(duration.toMillis)) >> fib.cancel
      result <- fib.join
    } yield result

    computationResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e)    => IO.raiseError(e)
      case Canceled() =>
        IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  def testEx3() = {
    val aComputation =
      IO("starting").debugIO >> IO.sleep(1.second) >> IO("done").debugIO >> IO(
        42
      )
    timeout(aComputation, 2.seconds).debugIO.void
  }

  override def run: IO[Unit] =
    // runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
    //   .debugIO.void

    // throwOnAnotherThread() // IO(Errored(new RuntimeException))
    //   .debugIO.void

    // testCancel().debugIO.void

    // processResultsFromFiber(
    //   IO(throw new IllegalStateException("bad"))
    // ).debugIO.void

    testEx3()
}
