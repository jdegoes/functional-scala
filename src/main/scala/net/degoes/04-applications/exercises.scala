// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.applications

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._

import scalaz._
import Scalaz._

object exercises extends App {
  //
  // EXERCISE 1
  //
  // Implement the `crawlIO` function.
  //
  def crawlIO[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Exception, Crawl[E, A]] = ???

  //
  // EXERCISE 2
  //
  // Implement a version of the `crawlIO` function that works in parallel.
  //
  def crawlIOPar[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Exception, Crawl[E, A]] = ???

  //
  // EXERCISE 3
  //
  // Implement a version of the `crawlIOPar` that can be tested without having
  // to interact with the real world.
  //
  def crawlIO2[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A],
    getURL    : URL => IO[Exception, String] = getURL(_)): IO[Exception, Crawl[E, A]] = ???

  final case class Crawl[E, A](error: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)
    def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
  }
  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]]{
        def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
        def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.error |+| r.error, l.value |+| r.value)
      }
  }

  final case class URL private (url: String) {
    final def relative(page: String): Option[URL] = URL(url + "/" + page)
  }

  object URL {
    def apply(url: String): Option[URL] =
      scala.util.Try(new java.net.URI(url).parseServerAuthority()).toOption match {
        case None => None
        case Some(_) => Some(new URL(url))
      }
  }

  def getURL(url: URL): IO[Exception, String] =
    IO.syncException(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try({
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

      for {
        m   <- matches
        url <- URL(m).toList ++ root.relative(m).toList
      } yield url
    }).getOrElse(Nil)
  }

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Hello World!")
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}
