// Copyright(C) 2018-2019 John A. De Goes. All rights reserved.

package net.degoes.design
import java.sql.ResultSet
import java.net.URL

object composability {

  /**
   * EXERCISE 1
   *
   * Measure the composability of the `Async` data type.
   */
  trait Async[+A] {
    def onComplete(callback: Either[Throwable, A] => Unit): Unit
  }

  /**
   * EXERCISE 2
   *
   * Introduce a new composable data type (`Async2`) to model async
   * computations that builds on `Async`.
   */
  trait Async2[+A] extends Async[A] { self =>
    ???
  }

  /**
   * EXERCISE 3
   *
   * Measure the composability of this widget framework API.
   */
  trait WidgetFramework {
    type Widget[+S]

    def createContainer: Widget[Any]

    def createTextField(name: String, chars: Int): Widget[String]

    def getState[A](widget: Widget[A]): A

    def installChild(container: Widget[Any], child: Widget[Any]): Unit

    def createButton(label: String, onClick: Widget[Unit] => Unit): Widget[Unit]

    def displayWidget(widget: Widget[Any]): Unit

    def destroyWidget(widget: Widget[Any]): Unit
  }

  /**
   * EXERCISE 4
   *
   * Introduce a new composable data type to model widgets and refactor the
   * API to use this new data type.
   */
  trait WidgetFramework2 {
    ???
  }

  /**
   * EXERCISE 5
   *
   * Measure the composability of the email scheduler API.
   */
  final case class Email(from: String, to: String, subject: String, body: String)
  trait EmailScheduler {
    def sendOnDayOfWeek(d: java.time.DayOfWeek, emails: List[Email]): Unit

    def sendOnDayOfWeekAtTime(d: java.time.DayOfWeek, hour: Int, emails: List[Email]): Unit

    def sendEveryNWeeks(n: Int, d: java.time.DayOfWeek, emails: List[Email]): Unit

    // Assume many other methods for all possible delivery schedules.
  }

  /**
   * EXERCISE 6
   *
   * Introduce a new composable data type to model delivery schedules and
   * refactor the API to use this new data type.
   */
  sealed trait EmailSchedule
  trait EmailScheduler2 {}

  /**
   * EXERCISE 7
   *
   * Measure the composability of the drawing API.
   */
  trait Turtle {
    def turnLeft(): Unit

    def turnRight(): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit
  }

  /**
   * EXERCISE 8
   *
   * Introduce a new composable data type to model drawing instructions, and
   * refactor the API to use this new data type.
   */
  type Turtle2
}

object orthogonality {

  /**
   * EXERCISE 1
   *
   * Analyze the orthogonality of the operations modeled by the following data
   * type.
   */
  object pipeline1 {
    sealed trait Pipeline[+A] { self =>
      def map[B](f: A => B): Pipeline[B] = Pipeline.Map(self, f)

      def flatMap[B](f: A => Pipeline[B], parallelism: Int): Pipeline[B] = Pipeline.FlatMap(self, f, parallelism)
    }
    object Pipeline {
      final case class Map[A0, A](p: Pipeline[A0], f: A0 => A)                                 extends Pipeline[A]
      final case class FlatMap[A0, A](p: Pipeline[A0], f: A0 => Pipeline[A], parallelism: Int) extends Pipeline[A]
      final case class Emit[A](value: A)                                                       extends Pipeline[A]

      def succeed[A](a: A): Pipeline[A] = Emit(a)
    }

    def run[A](pipeline: Pipeline[A]): List[A] = ???
  }

  /**
   * EXERCISE 2
   *
   * Design an orthogonal set of operations that is equivalent in expressive
   * power.
   */
  object pipeline2 {
    sealed trait Pipeline[+A]

    def run[A](pipeline: Pipeline[A]): List[A] = ???
  }

}

object polymorphism {

  /**
   * EXERCISE 1
   *
   * Introduce more polymorphism into the following API.
   */
  def partition[A](list: List[A], by: A => Either[A, A]): (List[A], List[A]) = ???

  /**
   * EXERCISE 2
   *
   * Introduce more polymorphism into the following API.
   */
  trait API2[F[_]] {
    def choose[A](left: F[A], right: F[A]): F[A]
  }

  /**
   * EXERCISE 3
   *
   * Introduce more polymorphism into the following API.
   */
  def fallback[A, B](first: Either[A, B], second: Either[A, B]): Either[A, B] = ???

  /**
   * EXERCISE 4
   *
   * Introduce more polymorphism into the following API.
   */
  def reduce[A](f: (A, A) => A)(tuple: (A, A, A)): A = ???
}

object symmetry {

  /**
   * EXERCISE 1
   *
   * Improve the symmetry of the following API.
   */
  trait API1[F[_]] {
    def product[A, B](left: F[A], right: F[B]): F[(A, B)]
  }

  /**
   * EXERCISE 2
   *
   * Improve the symmetry of the following API.
   */
  trait API2[F[_]] {
    def unit: F[Unit]
  }

  /**
   * EXERCISE 3
   *
   * Improve the symmetry of the following API.
   */
  trait API3[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  /**
   * EXERCISE 4
   *
   * Improve the symmetry of the following API.
   */
  trait API4 {
    def rassoc[A, B, C](t: ((A, B), C)): (A, (B, C))
  }
}

object inference {

  /**
   * EXERCISE 1
   *
   * Introduce declaration-site covariance into the `Validation` data structure
   * so that the commented line will compile.
   */
  sealed trait Validation[A]
  object Validation {
    final case class Errors[A](errors: List[String]) extends Validation[A]
    final case class Success[A](value: A)            extends Validation[A]
  }
  sealed trait Form
  object Form {
    sealed trait Text  extends Form
    sealed trait Email extends Form
  }
  lazy val textValidation: Validation[Form.Text]       = ???
  def printResults(validation: Validation[Form]): Unit = ???
  // lazy val _ = printResults(textValidation)

  /**
   * EXERCISE 2
   *
   * Using the value `emptySet` below, try to define empty integer and string
   * sets, and note any difficulties you encounter.
   */
  lazy val emptySet                    = Set()
  lazy val emptyIntSet: Set[Int]       = ???
  lazy val emptyStringSet: Set[String] = ???

  /**
   * EXERCISE 3
   *
   * Implement `SplitSet.empty`.
   */
  trait SplitSet[-In, +Out] { self =>
    def fold[Z, Out1 >: Out](z: Z)(f: (Z, Out1) => Z): Z

    def contains(in: In): Boolean =
      self.fold(false)((b, a) => b || a == in)

    def map[Out2](change: Out => Out2): SplitSet[In, Out2] =
      new SplitSet[In, Out2] {
        def fold[Z, Out1 >: Out2](z: Z)(f: (Z, Out1) => Z): Z =
          self.fold(z)((z, out) => f(z, change(out)))
      }
  }
  object SplitSet {
    implicit class SplitSetSyntaxI[A](self: SplitSet[A, A]) {
      def add(in: A): SplitSet[A, A] =
        new SplitSet[A, A] {
          def fold[Z, Out1 >: A](z: Z)(f: (Z, Out1) => Z): Z =
            f(self.fold(z)(f), in)
        }
    }
    lazy val empty: SplitSet[Any, Nothing] = ???
  }
  type ISet[A] = SplitSet[A, A]

  /**
   * EXERCISE 4
   *
   * Using the value `emptySet2` below, try to define empty integer and string
   * sets, and note any difficulties you encounter.
   */
  lazy val emptySet2                     = SplitSet.empty
  lazy val emptyIntSet2: ISet[Int]       = ???
  lazy val emptyStringSet2: ISet[String] = ???

  /**
   * EXERCISE 5
   *
   * Test the type inference of `foldLeft1` by uncommenting the code. Note
   * your findings below.
   */
  def foldLeft1[A, Z](list: List[A])(z: Z)(f: (Z, A) => Z): Z = list.foldLeft(z)(f)
  lazy val somes1: Option[Int] = foldLeft1(List(1, 2, 3, 4))(None) {
    case (_, a) => ??? // Some(a)
  }

  /**
   * EXERCISE 6
   *
   * Design a new signature for `foldLeft2` that does not have the type
   * inference problems of `foldLeft1`. You may change the order of
   * parameters or the number of parameter lists.
   */
  def foldLeft2[A, Z](list: List[A])(z: Z, f: (Z, A) => Z) = ???
  lazy val somes2: Option[Int]                             = foldLeft2(List(1, 2, 3, 4))(None, (_: Any, b) => Some(b))

  /**
   * EXERCISE 7
   *
   * Design a new signature for `foldLeft3` that does not have the type
   * inference problems of `foldLeft1`. You may not change the order of
   * parameters or the number of parameter lists.
   */
  def foldLeft3[A](list: List[A]): FoldLeft3[A] = ???
  lazy val somes3: Option[Int]                  = foldLeft3(List(1, 2, 3, 4))(None)((_: Any, b) => Some(b))

  class FoldLeft3_2[+A, Z](list: List[A], z: Z) {
    def apply[Z1 >: Z](f: (Z1, A) => Z1): Z1 = ???
  }

  class FoldLeft3[+A](list: List[A]) {
    def apply[Z](z: Z): FoldLeft3_2[A, Z] = new FoldLeft3_2(list, z)
  }
}

object performance {

  /**
   * EXERCISE 1
   *
   * Count the number of allocations in the following expression.
   */
  val solution1              = (1 :: 2 :: 3 :: 4 :: Nil).foldLeft(0)(_ + _)
  lazy val allocations1: Int = ???

  /**
   * EXERCISE 2
   *
   * Count the number of allocations in the following expression.
   */
  val solution2 = (1 :: 2 :: 3 :: 4 :: Nil).map(_ + 1).foldLeft(Option.empty[Int]) {
    case (None, _) => None
    case (Some(x), y) if x % 2 == 0 => Some(x + y)
    case (Some(x), _) => Some(x)
  }
  lazy val allocations2: Int = ???

  /**
   * EXERCISE 3
   *
   * Rewrite the solution in Exercise 2 with fewer allocations.
   */
  lazy val allocations3 = ???

  /**
   * EXERCISE 4
   *
   * Count the number of function applications in `solution1`.
   */
  lazy val allocations4: Int = ???

  /**
   * EXERCISE 5
   *
   * Count the number of function applications in `solution2`.
   */
  lazy val invocations5: Int = ???

  /**
   * EXERCISE 6
   *
   * Count the number of allocations and function applications.
   */
  def multiply1(left: List[Int], right: List[Int]): List[Int] =
    left.zip(right).map(t => t._1 * t._2)
  lazy val allocations6: Int = ???
  lazy val invocations6: Int = ???

  /**
   * EXERCISE 7
   *
   * Design an alternate solution for Exercise 6. You may not change the
   * types of the parameters.
   */
  def multiply2(left: List[Int], right: List[Int]): List[Int] = ???

  /**
   * EXERCISE 8
   *
   * Design an alternate solution for Exercise 6. You may change the types
   * of the parameters.
   */
  def multiply3 = ???
}

object projects {

  /**
   * EXERCISE 1
   *
   * Improve on the design of this API using everything you have learned so far.
   */
  trait Timeout
  trait Json
  trait HttpClient {
    def get(url: String, timeout: Timeout, retryCount: Int): Json

    def put(url: String, timeout: Timeout, body: Json, retryCount: Int): Json

    def post(url: String, timeout: Timeout, body: Json, retryCount: Int): Json

    def delete(url: String, timeout: Timeout, body: Json, retryCount: Int): Json

    def batchGet(reqs: List[(String, Timeout, Int)]): List[Json]

    def batchPut(reqs: List[(String, Timeout, Json, Int)]): List[Json]

    def batchPost(reqs: List[(String, Timeout, Json, Int)]): List[Json]

    def batchDelete(reqs: List[(String, Timeout, Json, Int)]): List[Json]
  }
}
