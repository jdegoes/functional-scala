package net.degoes.applications.configuration
import net.degoes.applications.TestRuntime

final class ConfigurationSpec extends TestRuntime {

  def is =
    s2"""
        Configuration.Live should work correctly $e
      """

  def e =
    load.provide(Configuration.Live).map { config =>
      config.api must_=== ApiConfig("127.0.0.1", 8080)
      config.dbConfig must_=== DbConfig("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "", "")
    }

}
