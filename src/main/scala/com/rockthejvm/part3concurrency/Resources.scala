package com.rockthejvm.part3concurrency

import cats.effect.IOApp
import cats.effect.IO
import com.rockthejvm.utils.debugIO
import scala.concurrent.duration._
import java.util.Scanner
import java.io.FileReader
import java.io.File
import scala.annotation.tailrec
import java.nio.file.Paths
import cats.effect.kernel.Resource
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

object Resources extends IOApp.Simple {

  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open(): IO[String] = IO(s"Opening connection to $url").debugIO
    def close(): IO[String] = IO(s"Closing connection to $url").debugIO
  }

  // incorrect way
  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open()) *> IO
      .sleep(
        (Int.MaxValue).seconds
      )
      .start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  // problem: leaking resources (conneciton is never closed)

  // correct way
  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib <- (conn.open() *> IO
      .sleep(
        (Int.MaxValue).seconds
      )).onCancel(conn.close().void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  // correct way (even better) - bracket pattern.
  // bracket pattern avoids the boilerplate that's needed if fetching / releasing resources gets messy
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn =>
      conn.close().void
    )

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /** Exercise: Read the file with the bracket pattern
    *   - open a scanner
    *   - read the file line by line, every 100 millis
    *   - close the scanner
    *   - if cancelled/throws error, close the scanner
    */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[Unit] = {

    // See the >> (lazy evaluation) instead of *> (eager evalutation).
    // With the latter we'll have a stackoverflow exception (since it does not wait for the first IO to finish)
    def readLineByLine(scanner: Scanner): IO[Unit] = {
      if (!scanner.hasNextLine()) IO.unit
      else {
        IO(scanner.nextLine()).debugIO >> IO.sleep(
          100.millis
        ) >> readLineByLine(scanner)
      }
    }

    IO(s"Opening file at $path").debugIO *> openFileScanner(path)
      .bracket(readLineByLine(_))(scanner =>
        IO(scanner.close()) *> IO(s"Closing file at $path").debugIO.void
      )
      .void
  }

  /** Resources
    */
  // get a connection from configuration (scanner for the conf file + connection itself)
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        // acquier a connection based on the file
        IO(new Connection(scanner.nextLine())).bracket { conn =>
          conn.open().debugIO >> IO.sleep(Int.MaxValue.millis)
        }(conn => conn.close().debugIO.void)
      }(scanner => IO("closing file").debugIO >> IO(scanner.close()))
  // this previous code starts being tedious when multiple resources have to be managed.

  // Resource
  // first, resource definition (open and close)
  val connectionResource =
    Resource.make(IO(new Connection("rockthejvm.com")))(conn =>
      conn.close().void
    )

  // second, resource usage
  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string =>
    IO(s"Using the string: $string").debugIO
  val releaseResource: String => IO[Unit] = string =>
    IO(s"finalizing the string: $string").debugIO.void

  val usingResourceWithBracket =
    simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource =
    Resource.make(simpleResource)(releaseResource).use(usingResource)

  /** Exercise: Read a text file with one line every 100 millis, using Resource
    * (refactor the bracket exercise to use Resource)
    */
  def connFromConfResource(path: String): Resource[IO, Connection] = {
    for {
      scanner <- Resource.make(
        IO("opening file").debugIO >> openFileScanner(path)
      )(scanner => IO("closing file").debugIO >> IO(scanner.close()).void)
      conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn =>
        conn.close().void
      )
    } yield conn
  }

  val openConnection = connFromConfResource(
    "src/main/scala/com/rockthejvm/part3concurrency/Resources.scala"
  ).use(conn => conn.open() >> IO.never)
  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(1.second) >> IO("cancelling!").debugIO >> fib.cancel
  } yield ()
  // connection + file will close automatically :)

  // finalizers to regular IOs
  val ioWithFinalizer =
    IO("some resource").debugIO.guarantee(IO("freeing resource").debugIO.void)

  val ioWithFinalizer_v2 = IO("some resource").debugIO.guaranteeCase {
    case Succeeded(fa) =>
      fa.flatMap(result => IO(s"releasing resource: $result").debugIO).void
    case Errored(e) => IO("nothing to release").debugIO.void
    case Canceled() =>
      IO("resource got canceled, releasing what's left").debugIO.void
  }

  def run = ioWithFinalizer_v2.void
  // canceledConnection
  // resourceReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
  // resourceFetchUrl.void
  // bracketReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
}
