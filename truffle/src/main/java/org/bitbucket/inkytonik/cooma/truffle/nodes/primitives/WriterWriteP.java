package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.bitbucket.inkytonik.cooma.truffle.CoomaConstants;
import org.bitbucket.inkytonik.cooma.truffle.exceptions.CapabilityException;
import org.bitbucket.inkytonik.cooma.truffle.exceptions.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Lazy;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.IntRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RowRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;

public final class WriterWriteP extends Primitive {

	private final Lazy<Writer> out;

	public WriterWriteP(String filename) throws CapabilityException {

		if (CoomaConstants.CONSOLEIO.equals(filename)) {
			this.out = Lazy.of(() -> new PrintWriter(getContext().get().getEnv().out()));
		} else {
			//TODO: the filename argument should be any resource, including a file on the web or a URI.
			if (!Files.isWritable(Paths.get(filename))) {
				throw new CapabilityException(String.format("Writer capability unavailable: can't write %s", filename));
			}
			this.out =  Lazy.of(() -> {
				try {
					return new BufferedWriter(new FileWriter(filename));
				} catch (IOException e) {
					throw new CoomaException(String.format("File %s not found", filename), this);
				}
			});
		}
	}

	@Override
	public Integer getNumargs() {
		return 1;
	}

	@Override
	public RuntimeValue run(Rho rho, String[] xs, String[] args) {
		RuntimeValue row = rho.get(xs[0]);
		//TODO: check permissions when the primitive is actually executed.
		if (row instanceof IntRuntimeValue || row instanceof StringRuntimeValue) {
			try (Writer outtry = out.get()) {
				outtry.write(row.toString());
			} catch (IOException e) {
				throw new CoomaException(e, this);
			}
		} else {
			throw new CoomaException(String.format("%s: can't write %s", getShow(), row.toString()), this);
		}
		return RowRuntimeValue.empty();
	}

	@Override
	public String getShow() {
		return String.format("consoleWrite %s", this.out);
	}
}
