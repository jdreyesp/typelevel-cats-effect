package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.util.Try
import com.rockthejvm.utils._
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import scala.concurrent.Await
import cats.effect.syntax.async
import scala.concurrent.duration._

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manunally manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Either[Throwable, Int] = Try {
    Thread.sleep(1000)
    println(
      s"[${Thread.currentThread().getName()}] computing the meaning of life on some other thread..."
    )
    42
  }.toEither

  // this is bad, since it returns Unit
  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLife())

  // lift computation to an IO
  // async brings a computation that's in another thread pool into cats effect thread pool
  val asyncMolIO: IO[Int] = IO.async_ {
    cb => // CatsEffect thread blocks (semantically) until this cb is invoked (by some other thread)
      threadPool.execute { () => // computation not managed by CatsEffect
        val result = computeMeaningOfLife()
        cb(result) // CatsEffect thread is notified with the result
      }
  }

  /** Exercise
    */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_ { cb =>
      ec.execute { () =>
        val result: Either[Throwable, A] = Try(computation()).toEither
        cb(result)
      }
    }

  /** Exercise
    */
  lazy val molFuture = Future { () => 42 }(ec)

  def futureToIO[A](computation: => Future[A])(ec: ExecutionContext): IO[A] =
    IO.async_ { cb =>
      computation.onComplete {
        case Success(value)     => cb(Right(value))
        case Failure(exception) => cb(Left(exception))
      }(ec)
    }

  def futureToIO_v2[A](computation: => Future[A])(ec: ExecutionContext): IO[A] =
    IO.async_ { cb =>
      computation.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }(ec)
    }

  val futureToIO_v3: IO[Int] = IO.fromFuture(IO(Future { 42 }(ec)))

  /** Exercise: A never-ending IO?
    */
  val neverEndingIO: IO[Int] = IO.async_(_ => ())
  val neverEndingIO_v2: IO[Int] = IO.never

  /*
  FULL ASYNC CALL - you get more control on when the computation is cancelled
   */
  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      // return IO[Option[IO[Unit]]]
      // we want a finalizer in case the computation gets cancelled
      // finalizers are of type IO[Unit]
      // Not specifying the finalizer => we need an Option[IO[Unit]]
      // creating option is an effect => IO[Option[IO[Unit]]]
      IO {
        threadPool
          .execute { () =>
            val result = computeMeaningOfLife()
            cb(result)
          }
      }.as(Some(IO("Cancelled!").debugIO.void))
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("Cancelling...").debugIO >> fib.cancel
      _ <- fib.join
      _ <- IO(threadPool.shutdown())
    } yield ()
  }

  override def run: IO[Unit] =
    demoAsyncCancellation()
    // futureToIO(Future { 42 }(ec))(ec).debugIO >> IO(threadPool.shutdown())
}
