package org.bitbucket.inkytonik.cooma.truffle;

import org.bitbucket.inkytonik.cooma.Config;
import org.bitbucket.inkytonik.cooma.CoomaParserSyntax;
import org.bitbucket.inkytonik.cooma.Driver;
import org.bitbucket.inkytonik.cooma.REPL;
import org.bitbucket.inkytonik.cooma.SymbolTable;
import org.bitbucket.inkytonik.cooma.SemanticAnalyser;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.kiama.relation.Tree;
import org.bitbucket.inkytonik.kiama.relation.LeaveAlone$;
import org.bitbucket.inkytonik.kiama.util.Message;
import org.bitbucket.inkytonik.kiama.util.Source;

public class TruffleDriver extends Driver {

    private CoomaTermNode currentCompiledNode;

    @Override
    public REPL createREPL(Config config) {
        return TruffleReplFrontendHolder.repl(config);
    }

    @Override
    public void process(Source source, CoomaParserSyntax.Program program, Config config) {
        TruffleCompiler compiler = new TruffleCompiler(config);
        currentCompiledNode  = compiler.compileCommand(program);
    }

    public CoomaTermNode getCurrentCompiledNode() {
        return currentCompiledNode;
    }

    public void setCurrentCompiledNode(CoomaTermNode currentCompiledNode) {
        this.currentCompiledNode = currentCompiledNode;
    }

}
