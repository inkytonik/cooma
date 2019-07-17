package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.bitbucket.inkytonik.cooma.truffle.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.IntRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RowRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public final class ConsoleWriteP extends Primitive {

    private final String filename;

    public ConsoleWriteP(String filename) {
        //TODO: change this for a IO stream
        this.filename = filename;
    }

    @Override
    public Integer getNumargs() {
        return 1;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {
        String x = xs[0];
        RuntimeValue row = rho.get(x);
        String s;
        if (row instanceof IntRuntimeValue || row instanceof StringRuntimeValue) {
            s = row.toString();
        } else {
            throw new CoomaException(String.format("%s: can't write %s", getShow(), row.toString()), this);
        }

        try {
            Files.write(Paths.get(filename), s.getBytes());
        } catch (IOException e) {
            throw new CoomaException(e.getCause(), this);
        }

        return RowRuntimeValue.empty();
    }

    @Override
    public String getShow() {
        return String.format("consoleWrite %s", this.filename);
    }
}
