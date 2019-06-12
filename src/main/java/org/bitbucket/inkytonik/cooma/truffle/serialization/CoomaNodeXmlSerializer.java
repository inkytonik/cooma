package org.bitbucket.inkytonik.cooma.truffle.serialization;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.graalvm.options.OptionType;

public final class CoomaNodeXmlSerializer {

    private static XStream xstream = new XStream(new DomDriver());

    public static String toXML(CoomaTermNode node){
        return xstream.toXML(node);
    }

    public static CoomaTermNode fromXML(String xml){
        return (CoomaTermNode) xstream.fromXML(xml);

    }

    public static OptionType CoomaOptionType = new OptionType<CoomaTermNode>(
            "CoomaNode",
            s -> CoomaNodeXmlSerializer.fromXML(s),
            coomaNode -> CoomaNodeXmlSerializer.toXML(coomaNode) );

}
