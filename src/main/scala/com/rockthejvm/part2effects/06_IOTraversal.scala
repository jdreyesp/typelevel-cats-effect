package com.rockthejvm.part2effects

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.Future
import scala.util.Random
import cats.Traverse
import com.rockthejvm.part1recap.CatsTypeClasses.listTraverse

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List(
    "I quite like CE",
    "Scala is great",
    "Looking forward to some awesome stuff"
  )

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    futures.foreach(_.foreach(println))
  }

  // Future[List[Int]] would be hard to obtain
  def traverseFutures(): Unit = {
    // traverse
    import cats.instances.list._
    val listTraverse = Traverse[List]
    val singleFuture: Future[List[Int]] =
      listTraverse.traverse(workLoad)(
        heavyComputation
      ) // this stores all the results

    singleFuture.foreach(println)
  }

  import com.rockthejvm.utils._

  // for IO it's the same
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debugIO

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /* Exercises */
  // use traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = {
    Traverse[List].traverse(listOfIOs)(identity)
  }

  // hard
  def sequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] = {
    Traverse[F].traverse(wrapperOfIOs)(identity)
  }

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = {
    import cats.syntax.traverse._
    listOfIOs.parTraverse(identity)
  }

  def parSequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    import cats.syntax.traverse._
    wrapperOfIOs.parTraverse(identity)

  override def run: IO[Unit] =
    // singleIO.map(_.sum).debugIO.void // sequential traversal
    // parallelSingleIO.map(_.sum).debugIO.void
    // sequence(List(IO(1), IO(2))).debugIO.void
    // parSequence(ios).map(_.sum).debugIO.void
    ios.parSequence.debugIO.void // this is the extension method from the Parallel syntax package
}
