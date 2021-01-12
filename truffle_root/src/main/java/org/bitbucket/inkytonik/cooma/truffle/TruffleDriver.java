/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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
        if (!config.usage().isSupplied()) {
            TruffleCompiler compiler = new TruffleCompiler(config);
            setCurrentCompiledNode(compiler.compileCommand(program));
        }
    }

    public CoomaTermNode getCurrentCompiledNode() {
        return currentCompiledNode;
    }

    public void setCurrentCompiledNode(CoomaTermNode currentCompiledNode) {
        this.currentCompiledNode = currentCompiledNode;
    }

}
