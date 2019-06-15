package net.degoes.applications

import java.util.{Timer, TimerTask}
import org.specs2.Specification
import org.specs2.execute.AsResult
import org.specs2.specification.core.{AsExecution, Execution}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scalaz.zio._

abstract class TestRuntime extends Specification with DefaultRuntime {
  val DefaultTimeout: Duration      = 60.seconds
  val timer                         = new Timer()
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  implicit def zioAsExecution[A: AsResult, R >: Environment, E]: AsExecution[ZIO[R, E, A]] =
    zio => Execution.withEnvAsync(_ => runToFutureWithTimeout(zio, DefaultTimeout))

  protected def runToFutureWithTimeout[E, R >: Environment, A: AsResult](
    zio: ZIO[R, E, A],
    timeout: Duration
  ): Future[A] = {
    val p = scala.concurrent.Promise[A]()
    val task = new TimerTask {
      override def run(): Unit =
        try {
          p.failure(new Exception("TIMEOUT: " + timeout))
          ()
        } catch {
          case _: Throwable => ()
        }
    }
    timer.schedule(task, timeout.toMillis)

    unsafeRunToFuture(zio.sandbox.mapError(FiberFailure).map(p.success).supervised)
    p.future
  }


}
