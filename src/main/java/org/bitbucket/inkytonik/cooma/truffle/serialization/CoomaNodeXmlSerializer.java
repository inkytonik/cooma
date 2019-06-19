package org.bitbucket.inkytonik.cooma.truffle.serialization;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

public final class CoomaNodeXmlSerializer {

    private static XStream xstream;

    static {
        xstream = new XStream(new JettisonMappedXmlDriver());
        xstream.setMode(XStream.NO_REFERENCES);
    }

    public static String toXML(CoomaTermNode node) {
        return xstream.toXML(node);
    }

    public static CoomaTermNode fromXML(String xml) {
        return (CoomaTermNode) xstream.fromXML(xml);
    }

}
