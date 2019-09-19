package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;

@Getter
@NodeInfo(shortName = "caseTerm", description = "A single case in casV")
public class CoomaCaseTerm extends CoomaNode {

    private final String c;
    private final String k;

    public CoomaCaseTerm(String c, String k) {
        this.c = c;
        this.k = k;
    }

}
