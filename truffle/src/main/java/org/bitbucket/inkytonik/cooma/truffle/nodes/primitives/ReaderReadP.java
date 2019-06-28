package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.bitbucket.inkytonik.cooma.truffle.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class ReaderReadP extends Primitive {

    private final String filename;

    public ReaderReadP(String filename) {
        this.filename = filename;
    }

    @Override
    public Integer getNumargs() {
        return 1;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {
        try {
            return new StringRuntimeValue(new String(Files.readAllBytes(Paths.get(this.filename))));
        } catch (IOException e) {
            throw new CoomaException(e.getCause(), this);
        }
    }

    @Override
    public String getShow() {
        return String.format("readerRead %s", this.filename);
    }
}
