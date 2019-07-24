package org.bitbucket.inkytonik.cooma.truffle.exceptions;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.nodes.Node;

public class CoomaException extends RuntimeException implements TruffleException {

    private static final long serialVersionUID = 1L;

    private final Node location;

    public CoomaException(String message, Node location) {
        super(message);
        this.location = location;
    }

    public CoomaException(Throwable throwable, Node location) {
        super(throwable);
        this.location = location;
    }

    @Override
    public Node getLocation() {
        return location;
    }
}
