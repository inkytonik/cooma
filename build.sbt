name := "cooma"
version := "0.1.0"
organization := "org.bitbucket.inkytonik.cooma"

scalaVersion := "2.12.8"

scalacOptions :=
    Seq (
        "-deprecation",
        "-feature",
        "-sourcepath", baseDirectory.value.getAbsolutePath,
        "-unchecked",
        "-Xfatal-warnings",
        "-Xcheckinit",
        "-Xlint:-stars-align,_"
    )

logLevel := Level.Info

shellPrompt := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

libraryDependencies ++=
    Seq (
        "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0-SNAPSHOT",
        "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0-SNAPSHOT" % "test" classifier ("tests"),
        "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0-SNAPSHOT",
        "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0-SNAPSHOT" % "test" classifier ("tests"),
        "org.scalatest" %% "scalatest" % "3.0.5" % "test",
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
    )

resolvers ++=
    Seq(
        Resolver.sonatypeRepo("snapshots")
    )

fork := true

ratsScalaRepetitionType := Some(VectorType)
ratsUseScalaOptions := true
ratsUseScalaPositions := true
ratsDefineASTClasses := true
ratsDefinePrettyPrinter := true
ratsUseKiama := 2
