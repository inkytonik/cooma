import sbt.Keys.{name, unmanagedSourceDirectories, version}
import sbtassembly.AssemblyPlugin.autoImport._
import scalariform.formatter.preferences._

import scala.sys.process._

name := "cooma"
version := "0.1.0"
organization in ThisBuild := "org.bitbucket.inkytonik.cooma"
scalaVersion in ThisBuild := "2.13.1"

lazy val kiamaDependencies = Seq(
	"org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0-SNAPSHOT",
	"org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0-SNAPSHOT" % "test" classifier ("tests"),
	"org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0-SNAPSHOT",
	"org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0-SNAPSHOT" % "test" classifier ("tests")
)

lazy val commonsettings = Seq(
	organization := "org.bitbucket.inkytonik.cooma",
	scalaVersion := "2.13.1",

	unmanagedSourceDirectories in Compile += baseDirectory.value / "truffle/target/scala-2.13/classes/org/bitbucket/inkytonik/cooma/truffle",
	scalacOptions ++=
		Seq(
			"-deprecation",
			"-feature",
			"-sourcepath", baseDirectory.value.getAbsolutePath,
			"-unchecked",
			"-Xfatal-warnings",
			"-Xcheckinit",
			"-Xlint:-stars-align,_"
		),
	resolvers ++= Seq (
		Resolver.sonatypeRepo ("releases"),
		Resolver.sonatypeRepo ("snapshots")
	),

	buildInfoKeys := Seq[BuildInfoKey](name, version),
	buildInfoPackage := organization.value,
	compileOrder := CompileOrder.Mixed,
	logLevel := Level.Info,
	shellPrompt := {
		state =>
			Project.extract(state).currentRef.project + " " + version.value +
				" " + scalaVersion.value + "> "
	},
	testOptions in Test := Seq(Tests.Argument(TestFrameworks.JUnit, "-a")),
	logBuffered in Test := false,
	//fork := true,
	connectInput in run := true,
	outputStrategy in run := Some(StdoutOutput),

	// sbt-rats
	ratsScalaRepetitionType := Some(VectorType),
	ratsUseScalaOptions := true,
	ratsUseScalaPositions := true,
	ratsDefineASTClasses := true,
	ratsDefinePrettyPrinter := true,
	ratsUseKiama := 2,

	// ScalariForm
	scalariformPreferences := scalariformPreferences.value
		.setPreference(AlignSingleLineCaseStatements, true)
		.setPreference(DanglingCloseParenthesis, Force)
		.setPreference(IndentSpaces, 4)
		.setPreference(SpaceBeforeColon, true)
		.setPreference(SpacesAroundMultiImports, false),

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
	)),
	updateOptions := updateOptions.value.withCachedResolution(true)
)

// Assembly
lazy val assemblySettings = Seq(
	assemblyJarName in assembly := name.value + ".jar",
	assemblyMergeStrategy in assembly := {
		case PathList("META-INF", "ECLIPSE_.RSA") => MergeStrategy.discard
		case PathList("META-INF", "truffle", "language") => MergeStrategy.first
		case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
		case PathList("META-INF", xs@_*) => MergeStrategy.first
		case x => MergeStrategy.first
	},
	test in assembly := {}
)

// Modules
lazy val root = (project in file("."))
	.settings(
		name := "cooma",
		version := "0.1.0",
		assemblySettings,
		commonsettings,
		mainClass in Compile := (mainClass in Compile in reference).value,
		libraryDependencies ++= Seq(
			"org.scalatest" %% "scalatest" % "3.0.8" % "test",
			"org.scalatestplus" %% "scalatestplus-scalacheck" % "1.0.0-SNAP8" % "test",
			"org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
		) ++ kiamaDependencies
	)
	.dependsOn(
		commons,
		reference,
		truffle,
		truffle_root,
		trufflelauncher,
		trufflecomponent
	)
	.aggregate(
		commons,
		reference,
		truffle,
		truffle_root,
		trufflelauncher,
		trufflecomponent
	)

lazy val reference = (project in file("reference"))
	.settings(
		commonsettings,
		mainClass in(Compile, run) := Some("org.bitbucket.inkytonik.cooma.Main")
	) dependsOn (commons)

lazy val truffle_root = (project in file("truffle_root"))
	.settings(
		assemblySettings,
		commonsettings
	)
    .aggregate(truffle)
	.dependsOn(truffle)

lazy val truffle = (project in file("truffle"))
	.settings(
		assemblySettings,
		commonsettings,
		compileOrder := CompileOrder.Mixed,
		libraryDependencies ++= Seq(
				"org.projectlombok" % "lombok" % "1.16.16",
				"org.graalvm.truffle" % "truffle-api" % "19.0.0",
				"org.graalvm.truffle" % "truffle-dsl-processor" % "19.0.0",
				"com.thoughtworks.xstream" % "xstream" % "1.4.3",
				"org.codehaus.jettison" % "jettison" % "1.3.7",
				"org.apache.commons" % "commons-lang3" % "3.9"
			)
	).dependsOn(commons)

lazy val commons = (project in file("commons"))
	.enablePlugins(BuildInfoPlugin)
	.settings(
		commonsettings,
		libraryDependencies ++= kiamaDependencies)

lazy val trufflelauncher = (project in file("truffle-launcher"))
	.settings(
		assemblySettings,
		libraryDependencies ++=
			Seq("org.graalvm.sdk" % "graal-sdk" % "19.0.0")
	)

lazy val buildComponent = taskKey[Unit]("Generates the component jar for GraalVM installation.")

lazy val installGraalVMComponent = taskKey[Unit]("Installs the generated component in the GraalVM")

lazy val trufflecomponent = (project in file("truffle-component"))
	.settings(
		buildComponent := {
			val files = assembly.all(ScopeFilter(inProjects(truffle, trufflelauncher))).value
			baseDirectory.value + "/make_component.sh" !
		},
		installGraalVMComponent := {
			buildComponent.value
			baseDirectory.value + "/install_component.sh" !
		}
	)
