import sbt._

lazy val functionalScala = (project in file(".")).
  settings (
    name          := "Functional Scala",
    organization  := "net.degoes",
    version       := "0.1-SNAPSHOT",
    scalaVersion  := "2.12.10",
    initialCommands in Compile in console := """
                                               |import scalaz._
                                               |import net.degoes._
    """.stripMargin
)

scalaVersion := "2.12.10"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding",
  "UTF-8",
  "-Xlint",
  "-Xverify",
  "-feature",
  "-Ypartial-unification",
  "-Xlint:-unused",
  "-language:_"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.7", "-target", "1.7")

val ScalaZVersion      = "7.2.26"
val Http4sVersion      = "0.20.13"
val CirceVersion       = "0.12.3"
val CirceVersionExtras = "0.12.2"
val DoobieVersion      = "0.7.0-M5"
val ZIOVersion         = "1.0-RC4"
val PureConfigVersion  = "0.11.0"
val H2Version          = "1.4.199"

libraryDependencies ++= Seq(
  // -- testing --
  "org.scalacheck" %% "scalacheck"  % "1.13.4" % Test,
  "org.scalatest"  %% "scalatest"   % "3.0.8"  % Test,
  "org.specs2"     %% "specs2-core" % "4.3.2"  % Test,
  // Scalaz
  "org.scalaz" %% "scalaz-core" % ScalaZVersion,
  // ZIO
  "org.scalaz" %% "scalaz-zio"              % ZIOVersion,
  "org.scalaz" %% "scalaz-zio-interop-cats" % ZIOVersion,
  // Http4s
  "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
  "org.http4s" %% "http4s-circe"        % Http4sVersion,
  "org.http4s" %% "http4s-dsl"          % Http4sVersion,
  // Circe
  "io.circe" %% "circe-generic"        % CirceVersion,
  "io.circe" %% "circe-generic-extras" % CirceVersionExtras,
  // Doobie
  "org.tpolecat" %% "doobie-core" % DoobieVersion,
  "org.tpolecat" %% "doobie-h2"   % DoobieVersion,
  // log4j
  "org.slf4j" % "slf4j-log4j12" % "1.7.26",
  //pure config
  "com.github.pureconfig" %% "pureconfig" % PureConfigVersion,
  //h2
  "com.h2database" % "h2" % H2Version
)

resolvers ++= Seq(
  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
  "Secured Central Repository" at "https://repo1.maven.org/maven2",
  Resolver.sonatypeRepo("snapshots")
)