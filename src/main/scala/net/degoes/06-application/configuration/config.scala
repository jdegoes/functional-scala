package net.degoes.applications.configuration

case class Config(api: ApiConfig, dbConfig: DbConfig)
case class ApiConfig(endpoint: String, port: Int)
case class DbConfig(
  url: String,
  user: String,
  password: String
)
