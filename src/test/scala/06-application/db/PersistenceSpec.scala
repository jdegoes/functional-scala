package net.degoes.applications.db

import data.User
import example.db.TestRuntime

import net.degoes.applications.configuration._

class PersistenceSpec extends TestRuntime {

  def is =
    s2"""
      * Persistence Live $e
    """

  def e =
    mkTransactor(DbConfig("jdbc:h2:~/test", "", ""), Platform.executor.asEC, ec).use {
      transaction =>
        (for {
          notFound <- get(100).either
          created  <- create(User(13, "usr")).either
          deleted  <- delete(13).either
        } yield
          (notFound.isLeft must beTrue) and (created must be right User(13, "usr")) and (
            deleted must_=== Right(()) 
          )).provide(new Persistence.Live {
          def tnx = transaction
        })
    }

}
