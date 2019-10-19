lazy val supportedScalaVersions = List("2.12.4")

lazy val root = project.in(file(".")).
    aggregate(crossedJVM, crossedJS).
    settings(
      crossScalaVersions := Nil,
      publish / skip := true
    )

lazy val crossed = crossProject.in(file(".")).
    settings(

      name := "ptolemy",
      organization := "edu.holycross.shot",
      version := "0.0.1",
      scalaVersion := "2.12.4",
      licenses += ("GPL-3.0",url("https://opensource.org/licenses/gpl-3.0.html")),
      resolvers += Resolver.jcenterRepo,
      resolvers += Resolver.bintrayRepo("neelsmith", "maven"),
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.0.1" % "test",

        "org.wvlet.airframe" %%% "airframe-log" % "19.9.0",

        "edu.holycross.shot.cite" %%% "xcite" % "4.0.2"
      )

    ).
    jvmSettings(
    
      tutTargetDirectory := file("docs"),
      tutSourceDirectory := file("tut"),
      crossScalaVersions := supportedScalaVersions

    ).
    jsSettings(
      skip in packageJSDependencies := false,
      scalaJSUseMainModuleInitializer in Compile := true,
      crossScalaVersions := supportedScalaVersions
    )

lazy val crossedJS = crossed.js
lazy val crossedJVM = crossed.jvm.enablePlugins(TutPlugin)
