package com.rockthejvm.part1recap

/**   - Applicative
  *   - Functor
  *   - FlatMap
  *   - Monad
  *   - Apply
  *   - ApplicativeError/monadError
  *   - traverse
  */
object CatsTypeClasses extends App {

  // functor - "mappable" data structures
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*
  val listFunctor = Functor[List]

  // generalizable "mapping" APIs
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  // import extension methods
  import cats.syntax.functor._
  // Remember: `F[_]: Functor`` This is context bound in Scala 3 (in Scala 2, it would be an implicit declaration).
  // So this context bound expects an implicit of a functor (same as using functor: Functor[F])
  def increment_v2[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)

  // applicative
  // ability to "wrap" types
  trait MyApplicative[F[_]] extends MyFunctor[F] { // it extends Functor! (so it has a `map` function with it)
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList: List[Int] = applicativeList.pure(43) // List[Int] = List(43)

  import cats.syntax.applicative.* // importts the pure extension method for all types
  val aSimpleList_v2: List[Int] = 43.pure[List]

  // FlatMap - ability to chain multiple wrapper computations
  trait MyFlatMap[F[_]] extends MyFunctor[F] { // it extends Functor! (so it has a `map` function with it)
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList = FlatMap[List]

  import cats.syntax.flatMap._ // flatMap extension method for all types

  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    // we see that flatMap is available due to the extension method coming from `import cats.syntax.flatMap._`
    // but where does `map` comes from? That's due that FlatMap extends Functor (and Functor brings `map`)
    fa.flatMap(a => fb.map(b => (a, b)))

  // Monad - applicative + flatMap
  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] =
      flatMap(initialValue)(a =>
        pure(f(a))
      ) // see how we use `pure` from Applicative to generate A => F[B]
  }

  import cats.Monad
  val monadList = Monad[List]

  def crossProduct_v2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  /* In summary, we have:
   *
   * Functor -> FlatMap --->
   *          \             \
   *          Applicative ----> Monad
   */

  // ApplicativeError
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    // this holds error of any type E (whatever an error means to you) and wraps it to F[A]
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]
  val applicativeErrorEither = ApplicativeError[ErrorOr, String]
  val desirableValue: ErrorOr[Int] = applicativeErrorEither.pure(42)
  val failedValue: ErrorOr[Int] =
    applicativeErrorEither.raiseError[Int]("Something failed")
  // look that in both desirable and failed values we get an ErrorOr[Int], so we can combine them

  import cats.syntax.applicativeError.* // raiseError extension method

  val failedValue_2: ErrorOr[Int] = "Something failed".raiseError[ErrorOr, Int]

  // we have also a Monad type for errors
  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]

  // traverse
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }

  // turn nested wrappers inside out
  val listOfOption: List[Option[Int]] = List(Some(1), Some(2), Some(43))
  // sometimes it's better to wrap this the other way around (depending on your use case), i.e. Option[List[Int]]
  import cats.Traverse
  val listTraverse = Traverse[List]
  val optionList: Option[List[Int]] =
    listTraverse.traverse(List(1, 2, 3))(x => Option(x))

  import cats.syntax.traverse.*
  val optionList_v2: Option[List[Int]] = List(1, 2, 3).traverse(x => Option(x))
}
