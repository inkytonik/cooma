package org.bitbucket.inkytonik.cooma.truffle.runtime;

import de.uka.ilkd.pp.DataLayouter;
import de.uka.ilkd.pp.PrettyPrintable;
import lombok.Value;

@Value
public class FieldValueRuntime implements Comparable<FieldValueRuntime>, PrettyPrintable {
	private final String x;
	private final RuntimeValue v;

	@Override
	public String toString() {
		return String.format("%s = %s", x, v.print());
	}

	@Override
	public int compareTo(FieldValueRuntime fieldValueRuntime) {
		int xc = x.compareTo(fieldValueRuntime.getX());
		if (xc != 0) return xc;
		return this.v.equals(fieldValueRuntime.getV()) ? 0 : -1;
	}

	@Override
	public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
		l.print(x).print(" = ").print(v);
	}

}
