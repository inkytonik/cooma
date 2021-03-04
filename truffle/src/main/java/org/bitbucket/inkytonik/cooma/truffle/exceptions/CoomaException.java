package org.bitbucket.inkytonik.cooma.truffle.exceptions;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;

public class CoomaException extends AbstractTruffleException {

    public CoomaException(String message, Node location) {
        super(message, location);
    }

}
