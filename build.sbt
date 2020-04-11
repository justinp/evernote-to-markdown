resolvers += "sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
)
