package net.degoes

import scala.concurrent.duration._

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._

import scalaz._
import Scalaz._

object FunctionalScala extends App {
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
