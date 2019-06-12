package org.bitbucket.inkytonik.cooma.truffle;


import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Option;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.RootNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaRootNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaAppFTermNodeGen;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.serialization.CoomaNodeXmlSerializer;
import org.graalvm.options.OptionCategory;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.options.OptionStability;

@TruffleLanguage.Registration(id = CoomaLanguage.ID, name = "cooma", defaultMimeType = CoomaLanguage.MIME_TYPE,
        characterMimeTypes = CoomaLanguage.MIME_TYPE, contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
        fileTypeDetectors = CoomaFileDetector.class)
public class CoomaLanguage extends TruffleLanguage<CoomaContext> {

    public static final String ID = "cooma";
    public static final String MIME_TYPE = "application/x-cooma";
    public static final String RHO = "rho";
    public static final String HALT = "$halt";

    @Option(help = "AST root node.", category = OptionCategory.USER,
            stability = OptionStability.STABLE)
    static OptionKey<CoomaTermNode> ast = new OptionKey<CoomaTermNode>(null, CoomaNodeXmlSerializer.CoomaOptionType);

    @Override
    protected CoomaContext createContext(TruffleLanguage.Env env) {
        return new CoomaContext(env);
    }

    @Override
    protected boolean isObjectOfLanguage(Object object) {
        if (!(object instanceof TruffleObject)) {
            return false;
        }
        //TODO: add the rest of the checks as seen in SLLanguage

        return false;
    }

    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        RootNode evalMain = new CoomaRootNode(this, getCurrentContext(this.getClass()).getEnv().getOptions().get(ast));
        return Truffle.getRuntime().createCallTarget(evalMain);
    }

    @Override
    protected OptionDescriptors getOptionDescriptors() {
        return new CoomaLanguageOptionDescriptors();
    }
}
