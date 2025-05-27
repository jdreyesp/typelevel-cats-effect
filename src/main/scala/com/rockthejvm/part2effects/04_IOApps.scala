package com.rockthejvm.part2effects

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import scala.io.StdIn

val program: IO[Unit] = for {
  line <- IO(StdIn.readLine())
  _ <- IO(println(s"You've just written: $line"))
} yield ()

object IOApps extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}

object MySimpleApp extends IOApp.Simple {
  override def run: IO[Unit] = program

}
