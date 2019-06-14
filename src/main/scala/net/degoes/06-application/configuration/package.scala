package net.degoes.applications

import doobie.h2.H2Transactor
import pureconfig.loadConfigOrThrow
import scala.concurrent.ExecutionContext
import scalaz.zio.{ Managed, Reservation, Task, ZIO }
import pureconfig.generic.auto._

package object configuration {
  def loadConfig: Task[Config] = Task.effect(loadConfigOrThrow[Config])

  def mkTransactor(
    conf: DbConfig,
    connectEC: ExecutionContext,
    transactEC: ExecutionContext
  ): Managed[Throwable, H2Transactor[Task]] = {
    import scalaz.zio.interop.catz._

    val xa = H2Transactor
      .newH2Transactor[Task](conf.url, conf.user, conf.password, connectEC, transactEC)

    val res = xa.allocated.map {
      case (transactor, cleanupM) =>
        Reservation(ZIO.succeed(transactor), cleanupM.orDie)
    }.uninterruptible

    Managed(res)
  }
}
