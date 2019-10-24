package net.degoes.applications.configuration
import pureconfig.loadConfigOrThrow
import scalaz.zio.{ Task, TaskR }
import pureconfig.generic.auto._

case class Config(api: ApiConfig, dbConfig: DbConfig)
case class ApiConfig(endpoint: String, port: Int)
case class DbConfig(
  url: String,
  user: String,
  password: String
)

trait Configuration extends Serializable {
  val config: Configuration.Service[Any]
}

object Configuration {
  trait Service[R] {
    val load: TaskR[R, Config]
  }

  trait Live extends Configuration {
    val config: Service[Any] = new Service[Any] {
      val load: Task[Config] = Task.effect(loadConfigOrThrow[Config])
    }
  }

  object Live extends Live
}
