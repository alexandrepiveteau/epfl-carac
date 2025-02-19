inThisBuild(Seq(
  organization := "ch.epfl.lamp",
  scalaVersion := "3.3.1-RC1-bin-20230313-f28d708-NIGHTLY",
//  scalaVersion := "3.3.1-RC1-bin-SNAPSHOT",
  version := "0.1",
))

lazy val root = project.in(file("."))
  .enablePlugins(PackPlugin)
  .settings(
    name := "datalog",

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      "org.scalameta" %% "munit" % "0.7.29" % Test,
    ),
  )

lazy val bench = project.in(file("bench"))
  .dependsOn(root)
  .dependsOn(root % "test->test")
  .enablePlugins(JmhPlugin)
  .settings(
    Jmh/sourceDirectory := (Test/sourceDirectory).value,
    Jmh/classDirectory := (Test/classDirectory).value,
    Jmh/dependencyClasspath := (Test/dependencyClasspath).value,
    // rewire tasks, so that 'jmh:run' automatically invokes 'jmh:compile' (otherwise a clean 'jmh:run' would fail)
    Jmh/compile := (Jmh/compile).dependsOn(Test/compile).value,
    Jmh/run := (Jmh/run).dependsOn(Jmh/compile).evaluated,

    // sbt-jmh generates a ton of Java files, but they're never referenced by Scala files.
    // By enforcing this using `compileOrder`, we avoid having to run these generated files
    // through the Scala typechecker which has a significant impact on compile-time.
    Jmh/compileOrder := CompileOrder.ScalaThenJava,
  )
