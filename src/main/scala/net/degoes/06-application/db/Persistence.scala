package net.degoes.applications.db

import net.degoes.applications.data.{ User, UserNotFound }
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
    val createTable: TaskR[R, Unit]
    def get(id: Int): TaskR[R, User]
    def create(user: User): TaskR[R, User]
    def delete(id: Int): TaskR[R, Unit]
  }

  /**
   * Persistence Module for production using Doobie
   */
  trait Live extends Persistence {

    protected def tnx: Transactor[Task]

    val userPersistence: Service[Any] = new Service[Any] {

      val createTable: Task[Unit] =
        SQL.createTable.run.transact(tnx).foldM(err => Task.fail(err), _ => Task.succeed(()))

      def get(id: Int): Task[User] =
        SQL
          .get(id)
          .option
          .transact(tnx)
          .foldM(
           Task.fail,
            maybeUser => Task.require(UserNotFound(id))(Task.succeed(maybeUser))
          )

      def create(user: User): Task[User] =
        SQL
          .create(user)
          .run
          .transact(tnx)
          .foldM(err => Task.fail(err), _ => Task.succeed(user))

      def delete(id: Int): Task[Unit] =
        SQL
          .delete(id)
          .run
          .transact(tnx)
          .foldM(err => Task.fail(err), _ => Task.succeed(()))
    }

    object SQL {

      def createTable: Update0 = sql"""CREATE TABLE IF NOT EXISTS Users (id int PRIMARY KEY, name varchar)""".update

      def get(id: Int): Query0[User] =
        sql"""SELECT * FROM USERS WHERE ID = $id """.query[User]

      def create(user: User): Update0 =
        sql"""INSERT INTO USERS (id, name) VALUES (${user.id}, ${user.name})""".update

      def delete(id: Int): Update0 =
        sql"""DELETE FROM USERS WHERE id = $id""".update
    }
  }

}
