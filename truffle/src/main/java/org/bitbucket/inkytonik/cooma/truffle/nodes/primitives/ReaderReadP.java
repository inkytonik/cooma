package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.bitbucket.inkytonik.cooma.CoomaConstants;
import org.bitbucket.inkytonik.cooma.exceptions.CapabilityException;
import org.bitbucket.inkytonik.cooma.truffle.exceptions.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.bitbucket.inkytonik.cooma.PrimitiveUtils.readReaderContents;

public class ReaderReadP extends Primitive {

    private final Reader in;

    public ReaderReadP(String filename) throws CapabilityException {
        if (CoomaConstants.CONSOLEIO.equals(filename)) {
            this.in = new BufferedReader( new InputStreamReader(getContext().get().getEnv().in()));
        } else {
            //TODO: the filename argument should be any resource, including a file on the web or a URI.
            if (!Files.isReadable(Paths.get(filename))) {
                throw new CapabilityException(String.format("Reader capability unavailable: can't read %s", filename));
            }
            try {
                this.in = new BufferedReader(new FileReader(filename));
            } catch (FileNotFoundException e) {
                throw new CoomaException(String.format("File %s not found", filename), this) ;
            }
        }
    }

    @Override
    public Integer getNumargs() {
        return 1;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {
        //TODO: check permissions when the primitive is actually executed.
        try {
            return new StringRuntimeValue(readReaderContents(in));
        } catch (IOException e) {
            throw new CoomaException(e.getCause(), this);
        }
    }

    @Override
    public String getShow() {
        return String.format("readerRead %s", this.in);
    }
}
