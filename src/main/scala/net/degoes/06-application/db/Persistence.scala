package db

import data.{ User, UserNotFound }
import doobie.{ Query0, Transactor, Update0 }
import scalaz.zio._
import doobie.implicits._
import scalaz.zio.interop.catz._

/**
 * Persistence Service
 */
trait Persistence extends Serializable {
  val userPersistence: Persistence.Service[Any]
}

object Persistence {
  trait Service[R] {
    def get(id: Int): TaskR[R, User]
    def create(user: User): TaskR[R, User]
    def delete(id: Int): TaskR[R, Boolean]
  }

  /**
   * Persistence Module for production using Doobie
   */
  trait Live extends Persistence {

    protected def tnx: Transactor[Task]

    val userPersistence: Service[Any] = new Service[Any] {

      def get(id: Int): Task[User] =
        SQL
          .get(id)
          .option
          .transact(tnx)
          .foldM(
            err => Task.fail(err),
            maybeUser => Task.require(UserNotFound(id))(Task.succeed(maybeUser))
          )

      def create(user: User): Task[User] =
        SQL
          .create(user)
          .run
          .transact(tnx)
          .foldM(err => Task.fail(err), _ => Task.succeed(user))

      def delete(id: Int): Task[Boolean] =
        SQL
          .delete(id)
          .run
          .transact(tnx)
          .fold(_ => false, _ => true)
    }

    object SQL {

      def get(id: Int): Query0[User] =
        sql"""SELECT * FROM USERS WHERE ID = $id """.query[User]

      def create(user: User): Update0 =
        sql"""INSERT INTO USERS (id, name) VALUES (${user.id}, ${user.name})""".update

      def delete(id: Int): Update0 =
        sql"""DELETE FROM USERS WHERE id = $id""".update
    }
  }

}
