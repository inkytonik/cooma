name := "cooma"
version := "0.1.0"
organization := "org.bitbucket.inkytonik.cooma"

scalaVersion := "2.12.8"

enablePlugins(BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](name, version)
buildInfoPackage := organization.value



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
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
    )

testOptions in Test := Seq(Tests.Argument(TestFrameworks.JUnit, "-a"))


resolvers ++=
    Seq(
        Resolver.sonatypeRepo("snapshots")
    )

fork := true

connectInput in run := true

outputStrategy in run := Some (StdoutOutput)

// sbt-rats

ratsScalaRepetitionType := Some(VectorType)
ratsUseScalaOptions := true
ratsUseScalaPositions := true
ratsDefineASTClasses := true
ratsDefinePrettyPrinter := true
ratsUseKiama := 2

// ScalariForm

import sbt.Keys.unmanagedSourceDirectories
import scalariform.formatter.preferences._
import xsbti.compile

scalariformPreferences := scalariformPreferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DanglingCloseParenthesis, Force)
    .setPreference(IndentSpaces, 4)
    .setPreference(SpaceBeforeColon, true)
    .setPreference(SpacesAroundMultiImports, false)

// License

// File headers

//  Use headerCheck to check which files need new headers
//  Use headerCreate in sbt to generate the headers
//  Use Test/headerCheck etc to do same in test code

headerLicense := Some(HeaderLicense.Custom(
   """|This file is part of Cooma.
      |
      |Copyright (C) 2019 Anthony M Sloane, Macquarie University.
      |
      |This Source Code Form is subject to the terms of the Mozilla Public
      |License, v. 2.0. If a copy of the MPL was not distributed with this
      |file, You can obtain one at http://mozilla.org/MPL/2.0/.
      |""".stripMargin
))

lazy val truffle = (project in file("truffle"))
  .settings(
    libraryDependencies ++=
      Seq (
        "org.projectlombok" % "lombok" % "1.16.16",
        "org.graalvm.truffle" % "truffle-api" %  "19.0.0",
        "org.graalvm.truffle" % "truffle-dsl-processor" %  "19.0.0",
        "com.thoughtworks.xstream" % "xstream" % "1.4.3",
        "org.codehaus.jettison" % "jettison" % "1.3.7",
        "org.apache.commons" % "commons-lang3" % "3.9"
      ),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "truffle/target/scala-2.12/classes/org/bitbucket/inkytonik/cooma/truffle/"
  )


lazy val root = project in file(".") dependsOn truffle
