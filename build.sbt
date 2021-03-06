name := "mockosaur"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.objenesis"  % "objenesis"  % "2.2"
libraryDependencies += "net.bytebuddy"  % "byte-buddy" % "0.7.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// deeper stacks in ScalaTest output
testOptions in Test += Tests.Argument("-oF")
