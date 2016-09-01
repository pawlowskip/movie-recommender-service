import sbt.Project.projectToRef

lazy val clients = Seq(client)
lazy val scalaV = "2.11.7"

lazy val commonDependencies = Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.7",
  "org.scala-lang" % "scala-library" % "2.11.7",
  "com.google.guava" % "guava" % "18.0",
  "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.4"
)

lazy val server = (project in file("server")).settings(
  scalaVersion := scalaV,
  scalaJSProjects := clients,
  pipelineStages := Seq(scalaJSProd, gzip),
  resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
  libraryDependencies ++= Seq(
    "com.vmunier" %% "play-scalajs-scripts" % "0.4.0",
    "org.reactivemongo" %% "play2-reactivemongo" % "0.12-RC2",
    specs2 % Test,
    ws % Test,
    "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
    "com.softwaremill.macwire" %% "macros" % "2.2.2" % "provided",
    "com.softwaremill.macwire" %% "util" % "2.2.2",
    "com.softwaremill.macwire" %% "proxy" % "2.2.2",
    "jp.t2v" %% "play2-auth" % "0.14.1",
    "jp.t2v" %% "play2-auth-test" % "0.14.1" % "test",
    "com.github.t3hnar" % "scala-bcrypt_2.11" % "2.4",
    play.sbt.Play.autoImport.cache
  ) ++ commonDependencies,
  includeFilter in (Assets, LessKeys.less) := "*.less",
  excludeFilter in (Assets, LessKeys.less) := "_*.less"
).enablePlugins(PlayScala, SbtWeb).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(sharedJvm)

lazy val client = (project in file("client")).settings(
  scalaVersion := scalaV,
  persistLauncher := true,
  persistLauncher in Test := false,
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.0",
    "com.lihaoyi" %%% "scalatags" % "0.4.6",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.10.4",
    "com.lihaoyi" %%% "utest" % "0.4.3" % "test",
    "org.webjars" % "jquery" % "1.11.1",
    "org.webjars" % "bootstrap" % "3.3.6",
    "org.webjars" % "font-awesome" % "4.5.0"
  )  ++ commonDependencies,
  jsDependencies ++= Seq(
    "org.webjars.bower" % "react" % "0.14.3"
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",
    "org.webjars.bower" % "react" % "0.14.3"
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM"
  )
).enablePlugins(ScalaJSPlugin, ScalaJSPlay).
  dependsOn(sharedJs)

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(
    scalaVersion := scalaV,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "0.4.0",
      "com.lihaoyi" %%% "utest" % "0.4.3" % "test"
    ) ++ commonDependencies,
    persistLauncher in Test := false,
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).jsConfigure(_ enablePlugins ScalaJSPlay)

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

// loads the Play project at sbt startup
onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value