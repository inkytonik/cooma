package org.bitbucket.inkytonik.cooma.truffle;

import org.bitbucket.inkytonik.cooma.Config;
import org.bitbucket.inkytonik.cooma.CoomaParserSyntax;
import org.bitbucket.inkytonik.cooma.Driver;
import org.bitbucket.inkytonik.cooma.REPL;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.kiama.util.Source;

public class TruffleDriver extends Driver {

    private CoomaTermNode currentCompiledNode;
    @Override
    public REPL createREPL(Config config) {
        return TruffleRepl.repl(config);
    }


    @Override
    public void process(Source source, CoomaParserSyntax.Program prog, Config config) {
        GraalVMCompiler compiler = new GraalVMCompiler(config);
        currentCompiledNode  = compiler.compileCommand(prog);
//        if (config.irPrint().isDefined())
//            config.output().apply().emitln(compiler.showTerm(currentCompiledNode));
//        if (config.irASTPrint().isDefined())
//            config.output().apply().emitln(
//                    JavaCoomaParserPrettyPrinter.getInstance().layout
//                            (JavaCoomaParserPrettyPrinter.getInstance().any(currentCompiledNode), 5));
    }

    public CoomaTermNode getCurrentCompiledNode() {
        return currentCompiledNode;
    }

    public void setCurrentCompiledNode(CoomaTermNode currentCompiledNode) {
        this.currentCompiledNode = currentCompiledNode;
    }
}
