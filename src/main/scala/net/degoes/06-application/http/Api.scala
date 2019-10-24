package net.degoes.applications.http

import io.circe.generic.auto._
import io.circe.{ Decoder, Encoder }
import net.degoes.applications.data.User
import net.degoes.applications.db.{ Persistence, _ }
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{ EntityDecoder, EntityEncoder, HttpRoutes }
import scalaz.zio._
import scalaz.zio.interop.catz._

final case class Api[R <: Persistence](rootUri: String) {

  type UserTask[A] = TaskR[R, A]

  implicit def circeJsonDecoder[A](implicit decoder: Decoder[A]): EntityDecoder[UserTask, A] = jsonOf[UserTask, A]
  implicit def circeJsonEncoder[A](implicit decoder: Encoder[A]): EntityEncoder[UserTask, A] =
    jsonEncoderOf[UserTask, A]

  val dsl: Http4sDsl[UserTask] = Http4sDsl[UserTask]
  import dsl._

  def route: HttpRoutes[UserTask] =
    HttpRoutes.of[UserTask] {
      case GET -> Root / IntVar(id) => get(id).foldM(_ => NotFound(), Ok(_))
      case request @ POST -> Root =>
        request.decode[User] { user =>
          Created(create(user))
        }
      case DELETE -> Root / IntVar(id) =>
        (get(id) *> delete(id)).foldM(_ => NotFound(), Ok(_))
    }

}
