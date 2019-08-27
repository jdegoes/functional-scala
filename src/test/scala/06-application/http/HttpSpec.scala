package net.degoes.applications.http

import io.circe.generic.auto._
import net.degoes.applications.configuration.DbConfig
import net.degoes.applications.data.User
import net.degoes.applications.db.Persistence
import net.degoes.applications.{TestRuntime, db}
import org.http4s.implicits._
import org.http4s.{Method, Request, Status, Uri}
import scalaz.zio._
import scalaz.zio.interop.catz._

final class HttpSpec extends TestRuntime {
  def is =
    s2"""
      Using Persistence.Live
        * Post User should work correctly    $e1
        * Get User should work correctly     $e2
        * Delete User should work correctly  $e3
      """

  def request[F[_]](method: Method, uri: String) = Request[F](method = method, uri = Uri.unsafeFromString(uri))

  val api = Api[Persistence]("users")
  val app = api.route.orNotFound
  import api._

  def e1 = {
    val user = User(42, "John")
    val req  = request[UserTask](Method.POST, "/").withEntity(user)
    provideLivePersistence(for {
      response   <- app.run(req)
      userResult <- response.as[User]
    } yield (response.status == Status.Created) && (userResult == user))
  }

  def e2 = {
    val user = User(42, "John")
    val req1 = request[UserTask](Method.POST, "/").withEntity(user)
    val req2 = request[UserTask](Method.GET, "/42")
    provideLivePersistence(for {
      _          <- app.run(req1)
      response   <- app.run(req2)
      userResult <- response.as[User]
    } yield (response.status == Status.Ok) && (userResult == user))
  }

  def e3 = {

    val user = User(42, "John")
    val req1 = request[UserTask](Method.POST, "/").withEntity(user)
    val req2 = request[UserTask](Method.DELETE, "/42")
    val req3 = request[UserTask](Method.GET, "/42")
    provideLivePersistence(for {
      _        <- app.run(req1)
      delete   <- app.run(req2)
      response <- app.run(req3)
    } yield delete.status == Status.Ok && response.status == Status.NotFound)
  }

  private def provideLivePersistence(zio: TaskR[Persistence, Boolean]): Task[Boolean] =
    Persistence.mkTransactor(DbConfig("jdbc:h2:mem:test", "", ""), Platform.executor.asEC, ec).use[Any, Throwable, Boolean] { transaction =>
      (db.createTable *> zio).provide(new Persistence.Live {
        def tnx = transaction
      })
    }.supervised

}
