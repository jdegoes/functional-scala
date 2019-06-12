package db
import data.User
import example.db.TestRuntime

class PersistenceSpec extends TestRuntime {

  def is =
    s2"""
      * Persistence Live $e
    """

  def e =
    configuration.mkTransactor(configuration.DbConfig("jdbc:h2:~/test", "", ""), Platform.executor.asEC, ec).use {
      transaction =>
        (for {
          notFound <- db.get(100).either
          created  <- db.create(User(13, "usr")).either
          deleted  <- db.delete(13).either
        } yield
          (notFound.isLeft must beTrue) and (created must be right User(13, "usr")) and (
            deleted must be right true
          )).provide(new Persistence.Live {
          def tnx = transaction
        })
    }

}
