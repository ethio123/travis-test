
name := """digit-recog"""

lazy val commonSettings = Seq(
    organization := "dele.book",
    version := "0.1.0",
    scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).settings(commonSettings: _*)
  .enablePlugins(PlayScala).aggregate(nnet, mateng).dependsOn(nnet, mateng)

lazy val nnet = (project in file("nnet")).settings(commonSettings: _*)
lazy val mateng = (project in file("mateng")).settings(commonSettings: _*)

scalaVersion := "2.11.8"
incOptions := incOptions.value.withNameHashing(true)
updateOptions := updateOptions.value.withCachedResolution(cachedResoluton = true)

routesGenerator := InjectedRoutesGenerator

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= {
    val ngVersion="2.0.0-rc.4"
    Seq(
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
        specs2 % Test,
        // from typescript template project
        cache,
        //angular2 dependencies

        "org.webjars.npm" % "angular__common" % ngVersion,
        "org.webjars.npm" % "angular__compiler" % ngVersion,
        "org.webjars.npm" % "angular__core" % ngVersion,
        "org.webjars.npm" % "angular__http" % ngVersion,
        "org.webjars.npm" % "angular__platform-browser-dynamic" % ngVersion,
        "org.webjars.npm" % "angular__platform-browser" % ngVersion,
        "org.webjars.npm" % "systemjs" % "0.19.31",
        "org.webjars.npm" % "todomvc-common" % "1.0.2",
        "org.webjars.npm" % "rxjs" % "5.0.0-beta.9",
        "org.webjars.npm" % "es6-promise" % "3.1.2",
        "org.webjars.npm" % "es6-shim" % "0.35.1",
        "org.webjars.npm" % "reflect-metadata" % "0.1.3",
        "org.webjars.npm" % "zone.js" % "0.6.12",
        "org.webjars.npm" % "core-js" % "2.4.0",
        "org.webjars.npm" % "symbol-observable" % "1.0.1",

        "org.webjars.npm" % "typescript" % "2.0.0",

        //tslint dependency
        "org.webjars.npm" % "tslint-eslint-rules" % "1.5.0",
        "org.webjars.npm" % "codelyzer" % "0.0.28",
        "org.webjars.npm" % "types__jasmine" % "2.2.26-alpha" % "test"
    )
}

dependencyOverrides += "org.webjars.npm" % "minimatch" % "3.0.0"
resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

// the typescript typing information is by convention in the typings directory
// It provides ES6 implementations. This is required when compiling to ES5.
typingsFile := Some(baseDirectory.value / "typings" / "index.d.ts")
// use the webjars npm directory (target/web/node_modules ) for resolution of module imports of angular2/core etc
resolveFromWebjarsNodeModulesDir := true

// use the combined tslint and eslint rules plus ng2 lint rules
(rulesDirectories in tslint) := Some(List(
    tslintEslintRulesDir.value,
    ng2LintRulesDir.value
))

routesGenerator := InjectedRoutesGenerator
