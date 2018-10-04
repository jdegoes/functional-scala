// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.applications

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._

import scalaz._
import Scalaz._

object exercises extends App {
  case class CrawlState[E, A](visited: Set[URL], crawl: Crawl[E, A])
  object CrawlState {
    def visited[E: Monoid, A: Monoid](visited: Set[URL]): CrawlState[E, A] =
      CrawlState(visited, mzero[Crawl[E, A]])
    def crawled[E, A](crawl: Crawl[E, A]): CrawlState[E, A] =
      CrawlState(mzero[Set[URL]], crawl)

    implicit def MonoidCrawlState[E: Monoid, A: Monoid]: Monoid[CrawlState[E, A]] =
      new Monoid[CrawlState[E, A]] {
        def zero = CrawlState(mzero[Set[URL]], mzero[Crawl[E, A]])
        def append(l: CrawlState[E, A], r: => CrawlState[E, A]) =
          CrawlState(l.visited |+| r.visited, l.crawl |+| r.crawl)
      }
  }
  //
  // EXERCISE 1
  //
  // Implement the `crawlIO` function.
  //
  def crawlIO[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
      def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
        ref.update(_ |+| CrawlState.visited(seeds)) *>
        IO.traverse(seeds)(seed =>
          getURL(seed).redeem(_ => IO.unit,
            html =>
              for {
                acc   <-  ref.get
                seeds <-  IO.now(extractURLs(seed, html).toSet.flatMap(router) -- acc.visited)
                crawl <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
                _     <-  ref.update(_ |+| CrawlState.crawled(crawl))
                _     <-  loop(seeds, ref)
              } yield ()
          )
        ).void

      for {
        ref   <- Ref(mzero[CrawlState[E, A]])
        _     <- loop(seeds, ref)
        state <- ref.get
      } yield state.crawl
    }

  //
  // EXERCISE 2
  //
  // Implement a version of the `crawlIO` function that works in parallel.
  //
  def crawlIOPar[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = ???

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
    getURL    : URL => IO[Exception, String] = getURL(_)): IO[Nothing, Crawl[E, A]] = ???

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
