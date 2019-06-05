package org.bitbucket.inkytonik.cooma.truffle;


import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.RootNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaRootNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.*;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaAppFTermNodeGen;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaDefTerm;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaIntValueNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;

@TruffleLanguage.Registration(id = CoomaLanguage.ID, name = "cooma", defaultMimeType = CoomaLanguage.MIME_TYPE,
        characterMimeTypes = CoomaLanguage.MIME_TYPE, contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
        fileTypeDetectors = CoomaFileDetector.class)
public final class CoomaLanguage extends TruffleLanguage<CoomaContext> {

    public static final String ID = "cooma";
    public static final String MIME_TYPE = "application/x-cooma";
    public static final String RHO = "rho";
    public static final String HALT = "halt";

    @Override
    protected CoomaContext createContext(Env env) {
        return null;
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
    protected CallTarget parse(TruffleLanguage.ParsingRequest request) throws Exception {


//        RootNode evalMain = new CoomaRootNode(this, new CoomaLetVTermNode(
//                    "x",
//                     new CoomaStringValueNode("hello world"), new CoomaHaltTermNode("x")));

        /*
        LetV(
                "f1",
                FunV("k2",
                        "x",
                        AppC("k2",
                                "x")
                ),

                LetV(
                        "x3",
                        IntV(10),
                        AppF("f1",
                                "halt",
                                "x3")
                )
        )
        */

//        RootNode evalMain = new CoomaRootNode(this,
//                new CoomaLetVTermNode("f1",
//                    new CoomaFuntionValueNode("k2", "x", new CoomaAppCTermNode("k2", "x")),
//                    new CoomaLetVTermNode("x3",
//                        new CoomaIntValueNode(10),
//                new CoomaAppFTermNode("f1", "halt", "x3"))));

        /*
        LetV(
            "f3",
            FunV(
                "k4",
                "x",
                LetV(
                    "f5",
                    FunV(
                        "j6",
                        "y",
                        AppC(
                            "j6",
                            "x")),
                    AppC(
                        "k4",
                        "f5"))
                ),
            LetV(
                "x7",
                IntV(10),
                LetC(
                    "k1",
                    "x2",
                    LetV(
                        "x8",
                        StrV("hello"),
                        AppF(
                            "x2",
                            "halt",
                            "x8")),
                    AppF(
                        "f3",
                        "k1",
                        "x7"))
                )
            )
         */

        /*


        RootNode evalMain = new CoomaRootNode(this,
                new CoomaLetVTermNode("f3",
                        new CoomaFuntionValueNode("k4", "x",
                                new CoomaLetVTermNode(
                                        "f5",
                                        new CoomaFuntionValueNode("j6", "y",
                                                new CoomaAppCTermNode("j6", "x")),
                                        new CoomaAppCTermNode("k4", "f5"))),
                        new CoomaLetVTermNode("x7",
                                new CoomaIntValueNode(10),
                                new CoomaLetCTermNode(
                                        "k1",
                                        "x2",
                                        new CoomaLetVTermNode("x8", new CoomaStringValueNode("hello"),
                                                 CoomaAppFTermNodeGen.create("x2", "halt", "x8")),
                                        CoomaAppFTermNodeGen.create("f3", "k1", "x7")
                                ))));
        */

        /*
        LetF(
            Vector(
                DefTerm(
                    "f",
                    "k1",
                    "x",
                    AppC(
                        "k1",
                        "x")),
                DefTerm(
                    "g",
                    "k2",
                    "y",
                    AppF(
                        "f",
                        "k2",
                        "y")
                    )
                ),
            LetV(
                "x5",
                IntV(
                    10),
                LetC(
                    "k3",
                    "x4",
                    AppC(
                        "$halt",
                        "x4"),
                    AppF(
                        "g",
                        "k3",
                        "x5")
                    )
                )
            )
         */

        RootNode evalMain = new CoomaRootNode(this,
                new CoomaLetFTermNode(
                        new CoomaDefTerm[]{
                                new CoomaDefTerm("f", "k1", "x", new CoomaAppCTermNode("k1", "x")),
                                new CoomaDefTerm("g", "k2", "y", CoomaAppFTermNodeGen.create("f", "k2", "y"))
                        },
                        new CoomaLetVTermNode("x5",
                                new CoomaIntValueNode(10),
                                new CoomaLetCTermNode(
                                        "k3",
                                        "x4",
                                        new CoomaHaltTermNode("x4"),
                                        CoomaAppFTermNodeGen.create("g", "k3", "x5")
                                )
                        )
                )
        );

        return Truffle.getRuntime().createCallTarget(evalMain);
    }
}
