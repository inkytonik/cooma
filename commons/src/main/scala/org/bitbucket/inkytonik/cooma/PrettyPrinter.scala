package org.bitbucket.inkytonik.cooma

object PrettyPrinter extends CoomaParserPrettyPrinter {

  override def toDoc(astNode: CoomaParserSyntax.ASTNode): Doc =
    link(astNode, super.toDoc(astNode))

}
