package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import com.rockthejvm.utils._
import scala.concurrent.duration._

object CancellingIOs extends IOApp.Simple {

  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation
   */
  val chainOfIOs: IO[Int] =
    IO("waiting").debugIO >> IO.canceled >> IO(42).debugIO

  // uncancelable - prevents being cancelled
  // example: online store, payment processor
  // payment process must NOT be canceled
  val specialPaymentSystem =
    (IO("Payment running, don't cancel me...").debugIO >>
      IO.sleep(1.second) >>
      IO("Payment completed.").debugIO)
      .onCancel(IO("MEGA CANCEL OF DOOM!").debugIO.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  // uncancelable 'mask'
  // take into account that we're not defining Poll[IO] (we define it as _). Explanation of it goes below
  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem)
  val atomicPayment_v2 = specialPaymentSystem.uncancelable // same

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO(
      "Attempting cancellation"
    ).debugIO >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
    The uncancellable API is more complex and more general.
    It takes a function from Poll[IO] to IO. In the example above, we're not using that Poll instance.
    The Poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   */

  /*
    Example: authentication service. It has two parts:
      - input password, can be cancelled, because otherwise we might block indefinitely on user input
      - verify password, CANNOT be cancelled once it's started
   */
  val inputPassword = IO("Input password:").debugIO >> IO(
    "(typing password)"
  ).debugIO >> IO.sleep(2.seconds) >> IO("RocktheJVM1!")
  val verifyPassword = (pw: String) =>
    IO("verifying...").debugIO >> IO.sleep(5.seconds) >> IO(
      pw == "RocktheJVM1!"
    )

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      // poll(...) unmasks this part (so this part IS cancellable)
      pw <- poll(inputPassword).onCancel(
        IO("Authentication timed out. Try again later.").debugIO.void
      )
      verified <- verifyPassword(pw)
      _ <-
        if (verified) IO("Authentication successful.").debugIO
        else IO("Authentication failed.").debugIO
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debugIO >> authFib.cancel // this won't be canceled
    _ <- authFib.join
  } yield ()

  /** Exercises
    */
  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debugIO
  val uncancelableMol =
    IO.uncancelable(_ => IO.canceled >> IO(42).debugIO) // what will this print?
  // Answer: uncancelable will eliminate ALL cancel points.

  // 2
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(1.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debugIO >> authFib.cancel // this won't be canceled
    _ <- authFib.join
  } yield ()

  // what do you think it will happen with this invincibleAuthProgram program?
  // Answer: Since uncancelable eliminates ALL cancel points, the underneath poll(inputPassword) will be uncancelable, so the
  // whole auth flow will be uncancelable

  // 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debugIO) >> IO.sleep(1.second) >>
        IO("uncancelable").debugIO >> IO.sleep(1.second) >>
        poll(IO("second cancelable").debugIO) >> IO.sleep(1.second)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("CANCELING").debugIO >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  // what do you think it will happen when cancellation occurs at that point in the code?
  // Answer: It will hit the uncancelable region, but the second cancelable region, since it was already canceled, it won't print.

  override def run = threeStepProgram().void
}
