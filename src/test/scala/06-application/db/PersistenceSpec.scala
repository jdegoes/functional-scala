package net.degoes.applications.db

import net.degoes.applications.TestRuntime
import net.degoes.applications.configuration._
import net.degoes.applications.data.{User, UserNotFound}

class PersistenceSpec extends TestRuntime {

  def is =
    s2"""
      * Persistence Live $e
    """


  def e =
    Persistence.mkTransactor(DbConfig("jdbc:h2:mem:test", "", ""), Platform.executor.asEC, ec).use { transaction =>
      (for {
        _        <- createTable
        notFound <- get(100).either
        created  <- create(User(13, "usr")).either
        deleted  <- delete(13).either
      } yield
        ((notFound == Left(UserNotFound(100))) must beTrue) and (created must be right User(13, "usr")) and (
          deleted.isRight must beTrue
        )).provide(new Persistence.Live {
        def tnx = transaction
      })
    }

}
