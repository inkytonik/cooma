/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle;

import org.bitbucket.inkytonik.cooma.Config;
import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Program;
import org.bitbucket.inkytonik.cooma.REPLDriver;
import org.bitbucket.inkytonik.cooma.REPL;
import org.bitbucket.inkytonik.cooma.SemanticAnalyser;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.kiama.util.Source;

public class TruffleDriver extends REPLDriver {

	private CoomaTermNode currentCompiledNode;
	private SemanticAnalyser analyser;

	@Override
	public REPL createREPL(Config config) {
		return TruffleReplFrontendHolder.repl(config, this);
	}

	@Override
	public void process(Source source, Program program, Config config) {
		try {
			if (!config.usage().isSupplied()) {
				TruffleCompiler compiler = new TruffleCompiler(config, analyser);
				setCurrentCompiledNode(compiler.compileCommand(program));
			}
		} catch (RuntimeException e) {
			e.printStackTrace();
			throw e;
		}
	}

	public CoomaTermNode getCurrentCompiledNode() {
		return currentCompiledNode;
	}

	public void setCurrentCompiledNode(CoomaTermNode currentCompiledNode) {
		this.currentCompiledNode = currentCompiledNode;
	}

	public void setAnalyser(SemanticAnalyser analyser) {
		this.analyser = analyser;
	}
}
