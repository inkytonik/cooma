package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

@Getter
@NodeInfo(shortName = "defTerm", description = "A single function definition in letF")
public class CoomaDefTerm extends CoomaNode {

    private final String f;
    private final String k;
    private final String x;

    @Child
    private CoomaTermNode body;

    public CoomaDefTerm(String f, String k, String x, CoomaTermNode body) {
        this.f = f;
        this.k = k;
        this.x = x;
        this.body = body;
    }

}
