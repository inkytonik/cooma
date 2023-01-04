package org.bitbucket.inkytonik.cooma.test

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.cooma.truffle.{TruffleDriver, TruffleFrontend}
import org.bitbucket.inkytonik.cooma.{Config, REPLDriver, ReferenceDriver}
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.{
  CompilerBase,
  Source,
  TestCompilerWithConfig
}
import org.rogach.scallop.throwError

import scala.collection.mutable

trait FileTests
    extends TestCompilerWithConfig[ASTNode, Program, Config]
    with CompilerBase[ASTNode, Program, Config] {

  val drivers = mutable.Map.empty[Config, REPLDriver]

  override def name: String = "cooma"

  override def makeast(
      source: Source,
      config: Config
  ): Either[Program, Messages] =
    createDriver(config).makeast(source, config)

  override def format(ast: Program): PrettyPrinterTypes.Document =
    (new ReferenceDriver).format(ast)

  override def createConfig(args: Seq[String]): Config = {
    // set Scallop so that errors don't just exit the process
    val saveThrowError = throwError.value
    throwError.value = true
    val config = new Config(args)
    config.verify()
    throwError.value = saveThrowError
    config
  }

  def createDriver(config: Config): REPLDriver =
    drivers.getOrElseUpdate(
      config,
      if (config.graalVM()) new TruffleDriver
      else new ReferenceDriver
    )

  def process(source: Source, prog: Program, config: Config): Unit = {
    val driver = createDriver(config)
    driver.process(source, prog, config)
  }

  override def testdriver(config: Config): Unit = {
    if (config.graalVM())
      (new TruffleFrontend).interpret(config)
    else
      createDriver(config).run(config)
  }

}
