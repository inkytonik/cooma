package org.bitbucket.inkytonik.cooma.exceptions;

public class CapabilityException extends Exception {

	public CapabilityException(String s) {
		super(s);
	}

	public CapabilityException(String s, Throwable throwable) {
		super(s, throwable);
	}

	public CapabilityException(Throwable throwable) {
		super(throwable);
	}
}
