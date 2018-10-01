// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz._
import Scalaz._

object exercises {
  object fixpoint {
    sealed trait Json[+A]
    object Json {
      case object Null extends Json[Nothing]
      case class Bool[A](boolean: Boolean) extends Json[A]
      case class Str[A](string: String) extends Json[A]
      case class Num[A](number: BigDecimal) extends Json[A]
      case class Arr[A](array: List[A]) extends Json[A]
      case class Obj[A](obj: Map[String, A]) extends Json[A]

      implicit val TraverseJson: Traverse[Json] = ???
    }
  }

  object selectable {
    sealed trait Parser[+E, +A] { self =>
      import Parser._

      def map[B](f: A => B): Parser[E, B] = Map[E, A, B](self, f)

      def || [E1 >: E, B](that: Parser[E1, B]): Parser[E1, Either[A, B]] =
        Alternative(self, that)

      def * : Parser[E, List[A]] = Repeat(self)

      def ~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, (A, B)] = Zip(self, that)

      def <~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, A] = (self ~ that).map(_._1)

      def ~> [E1 >: E, B](that: Parser[E1, B]): Parser[E1, B] = (self ~ that).map(_._2)
    }
    object Parser {
      def fail[E](e: E): Parser[E, Nothing] = Fail(e)

      def char[E](e: E): Parser[E, Char] = Character(e)

      def select[E, A](cond: Parser[E, Boolean])(
        ifTrue: Parser[E, A], ifFalse: Parser[E, A]): Parser[E, A] =
        Select(cond, ifTrue, ifFalse)

      case class Fail[E](error: E) extends Parser[E, Nothing]
      case class Succeed[A](value: A) extends Parser[Nothing, A]
      case class Character[E](error: E) extends Parser[E, Char]
      case class Repeat[E, A](value: Parser[E, A]) extends Parser[E, List[A]]
      case class Alternative[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, Either[A, B]]
      case class Zip[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, (A, B)]
      case class Map[E, A0, A](value: Parser[E, A0], f: A0 => A) extends Parser[E, A]
      case class Select[E, A](
        condition: Parser[E, Boolean], ifTrue: Parser[E, A], ifFalse: Parser[E, A]) extends Parser[E, A]

      implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
        new Applicative[Parser[E, ?]] {
          def point[A](a: => A): Parser[E,A] = Succeed(a)
          def ap[A, B](fa: => Parser[E,A])(f: => Parser[E,A => B]): Parser[E,B] =
            Map[E, (A => B, A), B](Zip(f, fa), t => t._1(t._2))
        }
    }

    def compiler[E, A](parser: Parser[E, A]): String => Either[E, A] =
      (input: String) => {
        var index: Int = 0
        var error: E = null.asInstanceOf[E]
        var value: A = null.asInstanceOf[A]
        type Repr = () => Unit

        def compile0(parser: Parser[E, A]): Repr = ???

        compile0(parser)()

        if (error != null) Left(error) else Right(value)
      }
  }
}
