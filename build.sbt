name := "bexprp"

organization := ""

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
	"org.specs2" %% "specs2" % "2.3.7" % "test"
)

//initialCommands := "import $organization$.$name;format="lower,word"$._"