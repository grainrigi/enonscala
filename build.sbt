scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")
javaOptions in run ++= Seq( "-Xmx2G", "-verbose:gc")
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"
