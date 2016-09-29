name := """digit-recog"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.8"

routesGenerator := InjectedRoutesGenerator

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
    "org.postgresql" % "postgresql" % "9.4.1207",
    "com.typesafe.play" %% "play-slick" % "2.0.0",
    "com.typesafe.play" %% "play-slick-evolutions" % "2.0.0",
    "com.typesafe.play" %% "play-mailer" % "5.0.0-M1",
    //"com.h2database" % "h2" % "1.4.187",
    "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % "test",
    "org.apache.pdfbox" % "pdfbox" % "1.8.11",
    "org.json4s" %% "json4s-native" % "3.3.0",
    "org.webjars" %% "webjars-play" % "2.5.0",
    "com.adrianhurt" %% "play-bootstrap3" % "0.4.5-P24",
    "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.4.2",
    specs2 % Test
)

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

