package net.degoes.applications

import data.User
import scalaz.zio.{ TaskR, ZIO }

/**
 * Helper that will access to the Persistence Service
 */
package object db extends Persistence.Service[Persistence] {
  val createTable: TaskR[Persistence, Unit]        = ZIO.accessM(_.userPersistence.createTable)
  def get(id: Int): TaskR[Persistence, User]       = ZIO.accessM(_.userPersistence.get(id))
  def create(user: User): TaskR[Persistence, User] = ZIO.accessM(_.userPersistence.create(user))
  def delete(id: Int): TaskR[Persistence, Unit]    = ZIO.accessM(_.userPersistence.delete(id))
}
