scalaVersion := "2.12.1"

parallelExecution in Test := false

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1"
libraryDependencies += "com.softwaremill.sttp.client" %% "core" % "2.0.7"
libraryDependencies += "org.bouncycastle" % "bcprov-jdk15on" % "1.65"
libraryDependencies += "com.softwaremill.sttp.client" %% "circe" % "2.0.9"
libraryDependencies += "io.circe" %% "circe-generic" % "0.12.3"
libraryDependencies += "name.neuhalfen.projects.crypto.bouncycastle.openpgp" % "bouncy-gpg" % "2.2.0"
// libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"



