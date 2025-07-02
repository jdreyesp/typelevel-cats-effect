package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.duration._
import com.rockthejvm.utils._
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executor
import java.util.concurrent.Executors

object BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.second).debugIO // SEMANTIC BLOCKING
    _ <- IO.sleep(1.second).debugIO
  } yield ()

  // really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName()}] computed a blocking code")
  } // will evaluate on a thread from ANOTHER thread pool specific for blocking calls

  // yielding - you can control how you yield the thread you're working on
  val iosOnManyThreads = for {
    _ <- IO("first").debugIO
    _ <-
      IO.cede // a signal to yield control over the thread (it's a hint) - equivalent to IO.shift in cats effect 2
    _ <- IO(
      "second"
    ).debugIO // the rest of this effect may run on another thread (not necessarily)
    _ <- IO.cede
    _ <- IO("third").debugIO
  } yield ()

  // Let's try to trick cats effect runtime by running this piece of code in a custom execution context (so cats effect
  // does not have control over), so thread switching may occur
  def testThousandEffectsSwitch() = {
    val executor = Executors.newFixedThreadPool(8)
    val ec: ExecutionContext =
      ExecutionContext.fromExecutor(executor)

    (1 to 1000)
      .map(IO.pure)
      .reduce(_.debugIO >> IO.cede >> _.debugIO)
      .evalOn(ec) >> IO(executor.shutdown())
  }

  override def run: IO[Unit] = testThousandEffectsSwitch().void
}
