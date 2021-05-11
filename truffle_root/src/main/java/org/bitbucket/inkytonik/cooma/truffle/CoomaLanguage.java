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

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.RootNode;
import org.bitbucket.inkytonik.cooma.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.Config;
import org.bitbucket.inkytonik.cooma.CoomaConstants;
import org.bitbucket.inkytonik.cooma.Util;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaRootNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermParser;
import org.bitbucket.inkytonik.cooma.truffle.runtime.*;
import org.bitbucket.inkytonik.kiama.util.Emitter;
import org.bitbucket.inkytonik.kiama.util.Positions;
import org.bitbucket.inkytonik.kiama.util.StringEmitter;

import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;

import xtc.parser.Column;
import xtc.parser.ParseError;
import xtc.parser.Result;
import xtc.parser.ParseException;

import static scala.collection.JavaConverters.collectionAsScalaIterableConverter;

@TruffleLanguage.Registration(id = CoomaConstants.ID, name = "cooma", defaultMimeType = CoomaConstants.MIME_TYPE,
		characterMimeTypes = CoomaConstants.MIME_TYPE, contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
		fileTypeDetectors = CoomaFileDetector.class, interactive = true)
public class CoomaLanguage extends TruffleLanguage<CoomaContext> {

	private TruffleDriver truffleDriver = new TruffleDriver();
	private CoomaContext context;
	private Boolean setInitialRho = true;

	@Override
	protected void finalizeContext(CoomaContext context) {
		System.setOut(context.getOriginalSout());
	}

	public static String toString(Object value) {
		try {
			if (value == null) {
				return "ANY";
			}
			InteropLibrary interop = InteropLibrary.getFactory().getUncached(value);
			if (interop.fitsInLong(value)) {
				return Long.toString(interop.asLong(value));
			} else if (interop.isBoolean(value)) {
				return Boolean.toString(interop.asBoolean(value));
			} else if (interop.isString(value)) {
				return interop.asString(value);
			} else if (interop.isNull(value)) {
				return "NULL";
			} else if (interop.hasMembers(value)) {
				return "Object";
			} else if (value instanceof RuntimeValue) {
				return ((RuntimeValue) value).print();
			} else {
				return "Unsupported";
			}
		} catch (UnsupportedMessageException e) {
			CompilerDirectives.transferToInterpreter();
			throw new AssertionError();
		}
	}

	@Override
	protected CoomaContext createContext(TruffleLanguage.Env env) {
		String[] args = env.getApplicationArguments();
		Config config = new Config(collectionAsScalaIterableConverter(Arrays.asList(args)).asScala().toSeq());
		Positions positions = new Positions();
		return new CoomaContext(env, new TruffleBackend(config), config);
	}

	@Override
	protected boolean isObjectOfLanguage(Object object) {
		if (!(object instanceof TruffleObject)) {
			return false;
		} else return RuntimeValue.class.isAssignableFrom(object.getClass());
	}

	@Override
	protected String toString(CoomaContext context, Object value) {
		return toString(value);
	}

	@Override
	protected CallTarget parse(ParsingRequest request) throws Exception {
		CoomaContext context = getCurrentContext(this.getClass());
		Config config = context.getConfig();
		String source = request.getSource().getCharacters().toString();
		if (source.isEmpty()) {
			compileFile(config);
		} else {
			truffleDriver.compileString("string source", source, config);
		}

		if (setInitialRho) {
			Rho preludeRho = preludeDynamicEnv(context, config);
			context.setRho(preludeRho);
			setInitialRho = false;
		}

		context.setApplicationArguments(Util.getConfigFilenamesTail(config));
		RootNode evalMain = new CoomaRootNode(this, context, truffleDriver.getCurrentCompiledNode());
		return Truffle.getRuntime().createCallTarget(evalMain);
	}

    private Rho preludeDynamicEnv(CoomaContext context, Config config) {
        if (config.noPrelude().isSupplied() || config.compilePrelude().isSupplied())
            return new Rho();
        else
            return readDynamicPrelude(config.preludePath().apply() + ".dynamic", context, config);
	}

    private Rho readDynamicPrelude(String filename, CoomaContext context, Config config) {
		try {
			FileReader reader = new FileReader(filename);
			CoomaTermParser p = new CoomaTermParser(reader, filename);
			p.setBackend(new TruffleBackend(config));
			Result pr = p.pDynamicPrelude(0);
			if (pr.hasValue()) {
				CoomaTermNode prelude = (CoomaTermNode)p.value(pr);
				context.setRho(new Rho());
				RootNode preludeRoot = new CoomaRootNode(this, context, prelude);
				CallTarget callTarget = Truffle.getRuntime().createCallTarget(preludeRoot);
				Object result = callTarget.call();
				return context.getRho();
			} else {
				ParseError error = pr.parseError();
				Column col = p.errorColumn(error);
				output(config, "cooma: can't parse dynamic prelude '" + filename + "'");
				output(config, filename + ":" + col.line + ":" + col.column + ": " + error.msg);
				System.exit(1);
			}
		}
		catch (FileNotFoundException e) {
			output(config, "cooma: can't find dynamic prelude '" + filename + "'");
			output(config, e.getMessage());
		}
		catch (IOException e) {
			output(config, "cooma: I/O error reading dynamic prelude '" + filename + "'");
			output(config, e.getMessage());
		}
		catch (ParseException e) {
			output(config, "cooma: xtc parse exception reading dynamic prelude '" + filename + "'");
			output(config, e.getMessage());
		}
		catch (CoomaException e) {
			CoomaException.errPrelude(e);
		}
		return new Rho();
    }

	private void output(Config config, String s) {
		config.output().apply().emitln(s);
	}

	protected void compileFile(Config config) throws CoomaFrontendException {
		truffleDriver.setCurrentCompiledNode(null);

		truffleDriver.compileFiles(config);

		// Check for frontend error
		if (truffleDriver.getCurrentCompiledNode() == null) {
			// If testing, capture output and pass back via exception
			// so can be reported by frontend. Can't be reported here
			// since config object is different to frontend. If not
			// testing, errors have already been reported.
			Emitter emitter = config.output().apply();
			String message =
					(emitter instanceof StringEmitter) ?
							((StringEmitter) emitter).result() :
							"";
			throw new CoomaFrontendException(message);
		}
	}

	@Override
	protected Object findMetaObject(CoomaContext context, Object value) {
		return getMetaObject(value);
	}

	public static String getMetaObject(Object value) {
		if (value == null) {
			return "ANY";
		}
		InteropLibrary interop = InteropLibrary.getFactory().getUncached(value);
		if (value instanceof IntRuntimeValue || interop.isNumber(value)) {
			return Type.Int.value;
		} else if (value instanceof StringRuntimeValue) {
			return Type.String.value;
		} else if (value instanceof ContinuationClosure) {
			return Type.Closure.value;
		} else if (value instanceof RecRuntimeValue) {
			return Type.Record.value;
		} else if (interop.isNull(value)) {
			return "NULL";
		} else if (interop.isExecutable(value)) {
			return "Function";
		} else if (interop.hasMembers(value)) {
			return "Object";
		} else {
			return "Unsupported";
		}
	}

	public enum Type {
		Int("Number"),
		String("String"),
		Record("Record"),
		Closure("Closure");

		final String value;

		Type(java.lang.String msg) {
			this.value = msg;
		}

		public java.lang.String getValue() {
			return value;
		}
	}

}
