package org.bitbucket.inkytonik.cooma;
import org.graalvm.polyglot.Context;


public class CoomaIRTruffleMain {
    public static void main(String[] args){
        try (Context context = Context.newBuilder("cooma")
                .arguments("cooma", args)
                .build()) {
            System.out.println(String.format("== running on %s - %s - %s"
                    , context.getEngine().getImplementationName()
                    , context.getEngine().getVersion()
                    , context.getEngine().getLanguages()
                    ));
            context.eval("cooma", "");
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
            System.out.println("Cooma language truffle implementation cannot be used standalone but only as a backend for the moment.");
            System.exit(-1);
        }

    }
}
