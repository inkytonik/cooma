import sbt.Keys.{name, unmanagedSourceDirectories, version}
import scalariform.formatter.preferences._

import scala.sys.process._

name := "cooma"
version := "0.1.0"
ThisBuild / organization := "org.bitbucket.inkytonik.cooma"
ThisBuild / scalaVersion := "2.13.9"

lazy val kiamaDependencies = Seq(
	"org.bitbucket.inkytonik.kiama" %% "kiama" % "2.4.0",
	"org.bitbucket.inkytonik.kiama" %% "kiama" % "2.4.0" % "test" classifier ("tests"),
	"org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.4.0",
	"org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.4.0" % "test" classifier ("tests")
)

Global/excludeLintKeys ++=
	Set(
		buildInfoKeys,
		buildInfoPackage
	)

lazy val commonsettings = Seq(
	organization := "org.bitbucket.inkytonik.cooma",
	scalaVersion := "2.13.9",

	Compile / unmanagedSourceDirectories += baseDirectory.value / "truffle/target/scala-2.13/classes/org/bitbucket/inkytonik/cooma/truffle",
	scalacOptions ++=
		Seq(
			"-deprecation",
			"-feature",
			"-sourcepath", baseDirectory.value.getAbsolutePath,
			"-unchecked",
			"-Xfatal-warnings",
			"-Xcheckinit",
			"-Xlint:-stars-align,_",
			"-Ypatmat-exhaust-depth", "40"
		),
	resolvers ++=
		Resolver.sonatypeOssRepos ("releases") ++
		Resolver.sonatypeOssRepos ("snapshots") ++
		Seq (
			Resolver.bintrayRepo("wolfendale", "maven")
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
	Test / testOptions := Seq(Tests.Argument(TestFrameworks.JUnit, "-a")),
	Test / logBuffered := false,
	fork := true,
	run / connectInput := true,
	run / outputStrategy := Some(StdoutOutput),
	run / javaOptions ++= Seq("-Xss16m", "-Xmx1G"),
	Test / javaOptions ++= Seq("-Xss16m", "-Xmx1G"),

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
		   |Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
		   |
		   |This Source Code Form is subject to the terms of the Mozilla Public
		   |License, v. 2.0. If a copy of the MPL was not distributed with this
		   |file, You can obtain one at http://mozilla.org/MPL/2.0/.
		   |""".stripMargin
	)),
	Test/headerLicense := headerLicense.value,
	updateOptions := updateOptions.value.withCachedResolution(true)
) ++ assemblySettings

// Assembly
lazy val assemblySettings = Seq(
	assembly / assemblyJarName := name.value + ".jar",
	assembly / assemblyMergeStrategy := {
		// case "module-info.class" => MergeStrategy.discard
		case PathList("META-INF", _*) => MergeStrategy.discard
		case _ => MergeStrategy.first
	},
	assembly / assemblyExcludedJars := {
		val cp = (fullClasspath in assembly).value
		cp filter { f =>
			//f.data.getName.contains("truffle-api") ||
			//f.data.getName.contains("truffle-tck")
			f.data.getName.contains("truffle-dsl-processor") ||
			f.data.getName.contains("graal-sdk") ||
			f.data.getName.contains("jackson")
		}
	},
	assembly / test := {},
	assembly / mainClass := Some("org.bitbucket.inkytonik.cooma.Main"),
	assembly / assembledMappings += {
		sbtassembly.MappingSet(None, Vector(
			(baseDirectory.value / "prelude" / "prelude.cooma.dynamic") -> "prelude/prelude.cooma.dynamic",
			(baseDirectory.value / "prelude" / "prelude.cooma.static") -> "prelude/prelude.cooma.static",
		))
	}
)

// Modules
lazy val root = (project in file("."))
	.settings(
		name := "cooma",
		version := "0.1.0",
		commonsettings,
		mainClass in Compile := (mainClass in Compile in reference).value,
		unmanagedResourceDirectories in Compile := Seq(baseDirectory.value / "prelude"),
		libraryDependencies ++= Seq(
			"org.http4s" %% "http4s-blaze-server" % "0.21.16" % "test",
			"org.http4s" %% "http4s-blaze-client" % "0.21.16" % "test",
			"org.http4s" %% "http4s-dsl" % "0.21.16" % "test",
            "org.scalatest" %% "scalatest" % "3.2.3" % "test",
			"org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % "test",
            "org.scalacheck" %% "scalacheck" % "1.15.2" % "test",
			"org.xerial" % "sqlite-jdbc" % "3.34.0",
			"io.github.wolfendale" %% "scalacheck-gen-regexp" % "0.1.3",
		) ++ kiamaDependencies,
		Test / fork := false,
		Test / parallelExecution := false
	)
	.dependsOn(
		commons,
		reference,
		truffle,
		truffle_root,
		trufflecomponent
	)
	.aggregate(
		commons,
		reference,
		truffle,
		truffle_root,
		trufflecomponent
	)

lazy val reference = (project in file("reference"))
	.settings(
		commonsettings,
		mainClass in(Compile, run) := Some("org.bitbucket.inkytonik.cooma.Main")
	) dependsOn (commons)
lazy val truffle_root = (project in file("truffle_root"))
	.settings(
		commonsettings
	)
    .aggregate(truffle)
	.dependsOn(truffle)

lazy val truffle = (project in file("truffle"))
	.settings(
		commonsettings,

		// sbt-rats
		ratsMainModule := Some ((Compile / scalaSource).value / "org" / "bitbucket" / "inkytonik" / "cooma" / "truffle" / "nodes" / "term" / "CoomaTermParser.rats"),

		compileOrder := CompileOrder.Mixed,
		libraryDependencies ++= Seq(
				"org.projectlombok" % "lombok" % "1.18.20",
				"org.graalvm.truffle" % "truffle-api" % "19.3.1",
				"org.graalvm.truffle" % "truffle-dsl-processor" % "19.3.1",
				"com.io7m.jpplib" % "com.io7m.jpplib.core" % "0.8.0"
			)
	).dependsOn(commons)

lazy val commons = (project in file("commons"))
	.enablePlugins(BuildInfoPlugin)
	.settings(
		commonsettings,

		// sbt-rats
		ratsScalaRepetitionType := Some(VectorType),
		ratsUseScalaOptions := true,
		ratsUseScalaPositions := true,
		ratsDefineASTClasses := true,
		ratsDefinePrettyPrinter := true,
		ratsUseKiama := 2,

		libraryDependencies ++= kiamaDependencies,
		libraryDependencies ++= Seq(
			"com.typesafe.play" %% "play-json" % "2.9.2",
			"org.scalaj" %% "scalaj-http" % "2.4.2"
		)
	)


lazy val buildComponent = taskKey[Unit]("Generates the component jar for GraalVM installation.")

lazy val installGraalVMComponent = taskKey[Unit]("Installs the generated component in the GraalVM")

lazy val trufflecomponent = (project in file("truffle-component"))
	.settings(
		buildComponent := {
			val files = assembly.all(ScopeFilter(inProjects(truffle_root))).value
			baseDirectory.value + "/make_component.sh" !
		},
		installGraalVMComponent := {
			// buildComponent.value
			baseDirectory.value + "/install_component.sh" !
		},
		headerLicense := (commons/headerLicense).value,
		Test/headerLicense := (commons/headerLicense).value
	)

// Custom tasks

val testFailing = taskKey[Unit]("Launch tests, tracing only failures")
testFailing := (Test / testOnly).toTask(" * -- -oC").value
