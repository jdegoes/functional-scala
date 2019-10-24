/**
 * This example is inspired by [[https://github.com/mschuwalow/zio-todo-backend]]
 */
package net.degoes.applications

import cats.effect.ExitCode
import net.degoes.applications.configuration.Configuration
import net.degoes.applications.db.Persistence
import net.degoes.applications.http.Api
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import scalaz.zio.blocking.Blocking
import scalaz.zio.clock.Clock
import scalaz.zio.console.putStrLn
import scalaz.zio.interop.catz._
import scalaz.zio.{ Task, TaskR, ZIO, _ }

object Main extends App {

  type AppEnvironment = Clock with Persistence

  type AppTask[A] = TaskR[AppEnvironment, A]

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = {
    val program: ZIO[Main.Environment, Throwable, Unit] = for {
      conf       <- configuration.load.provide(Configuration.Live)
      blockingEC <- blocking.blockingExecutor.map(_.asEC).provide(Blocking.Live)

      transactorR = Persistence.mkTransactor(
        conf.dbConfig,
        Platform.executor.asEC,
        blockingEC
      )

      httpApp = Router[AppTask](
        "/users" -> Api(s"${conf.api.endpoint}/users").route
      ).orNotFound

      server = ZIO.runtime[AppEnvironment].flatMap { implicit rts =>
        db.createTable *>
          BlazeServerBuilder[AppTask]
            .bindHttp(conf.api.port, "0.0.0.0")
            .withHttpApp(CORS(httpApp))
            .serve
            .compile[AppTask, AppTask, ExitCode]
            .drain
      }
      program <- transactorR.use { transactor =>
                  server.provideSome[Environment] { _ =>
                    new Clock.Live with Persistence.Live {
                      override protected def tnx: doobie.Transactor[Task] = transactor
                    }
                  }
                }
    } yield program

    program.foldM(
      err => putStrLn(s"Execution failed with: $err") *> IO.succeed(1),
      _ => IO.succeed(0)
    )
  }
}
