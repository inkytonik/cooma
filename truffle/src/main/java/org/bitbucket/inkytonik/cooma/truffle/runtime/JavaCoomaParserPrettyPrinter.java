package org.bitbucket.inkytonik.cooma.truffle.runtime;

import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinterImpl;

public class JavaCoomaParserPrettyPrinter extends CoomaParserPrettyPrinterImpl {

	private static JavaCoomaParserPrettyPrinter instance;

	public static JavaCoomaParserPrettyPrinter getInstance(){
		if (instance == null) {
			instance = new JavaCoomaParserPrettyPrinter();
		}
		return instance;
	}


}
