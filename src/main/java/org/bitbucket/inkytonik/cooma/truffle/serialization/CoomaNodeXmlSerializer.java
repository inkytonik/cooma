package org.bitbucket.inkytonik.cooma.truffle.serialization;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver;
import com.thoughtworks.xstream.io.json.JsonHierarchicalStreamDriver;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.graalvm.options.OptionType;

public final class CoomaNodeXmlSerializer {

    private static XStream xstreamW;
    private static XStream xstreamR;
    public static OptionType<CoomaTermNode> CoomaOptionType = new OptionType<>(
            "CoomaNode",
            CoomaNodeXmlSerializer::fromXML,
            CoomaNodeXmlSerializer::toXML);

    static {
        xstreamW = new XStream(new JsonHierarchicalStreamDriver());
        xstreamW.setMode(XStream.NO_REFERENCES);

        xstreamR  = new XStream(new JettisonMappedXmlDriver());
        xstreamR.setMode(XStream.NO_REFERENCES);
    }

    public static String toXML(CoomaTermNode node) {
        return xstreamR.toXML(node);
    }

    public static CoomaTermNode fromXML(String xml) {
        return (CoomaTermNode) xstreamR.fromXML(xml);
    }

}
