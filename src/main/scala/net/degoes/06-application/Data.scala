package net.degoes.applications.data

final case class User(id: Long, name: String)

final case class UserNotFound(id: Int) extends Exception
