// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._

object algebra {
  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] = ???
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //
  case class Permission()
  implicit val SemigroupPermission: Monoid[Permission] = ???
  val example2 = mzero[Permission] |+| Permission()


}

object functor {
  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] = ???

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  implicit val NothingFunctor: Functor[Nothing] = ???

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] = ???

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] = ???

  //
  // EXERCUSE 5
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  val example1 = (Option(3) |@| Option(5))((_, _))
  val example2 = zip(Option(3), Option(5))
  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] = ???

  //
  // EXERCISE 6
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] = ???

  //
  // EXERCISE 8
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] = ???

  //
  // EXERCISE 9
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] = ???
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
}

object optics {

}
