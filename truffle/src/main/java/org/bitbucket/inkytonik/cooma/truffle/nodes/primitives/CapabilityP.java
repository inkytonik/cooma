package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import lombok.RequiredArgsConstructor;
import lombok.val;
import org.bitbucket.inkytonik.cooma.truffle.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaAppCTermNodeGen;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaLetVTermNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaPrimitiveValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.*;

import java.nio.file.Files;
import java.nio.file.Paths;

import static org.bitbucket.inkytonik.cooma.Util.fresh;

@RequiredArgsConstructor
public class CapabilityP extends Primitive {

    final String cap;

    @Override
    public Integer getNumargs() {
        return 1;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {
        return capability(rho, cap, xs[0]);
    }


    private RuntimeValue capability(Rho rho, String name, String x) {

        val val = rho.get(x);
        String argument;
        if (val instanceof StringRuntimeValue) {
            argument = ((StringRuntimeValue) val).getInnerValue();
        } else if (val instanceof ErrorRuntimeValue) {
            return val;
        } else {
            throw new CoomaException(String.format("interpretPrim console: got non-String %s", String.valueOf(val)), this);
        }

        RuntimeValue result;
        switch (name) {

            case "Console":
                if (Files.isWritable(Paths.get(argument))) {
                    result = makeCapability("write", new ConsoleWriteP(argument));
                } else {
                    result = new ErrorRuntimeValue(String.format("Console capability unavailable: can't write %s", argument));
                }
                break;

            case "Reader":
                if (Files.isReadable(Paths.get(argument))) {
                    result = makeCapability("read", new ReaderReadP(argument));
                } else {
                    result = new ErrorRuntimeValue(String.format("Reader capability unavailable: can't read %s", argument));
                }
                break;
            default:
                throw new CoomaException(String.format("capability: unknown primitive %s", name), this);
        }

        return result;
    }

    private RowRuntimeValue makeCapability(String field, Primitive primitive) {
        val k = fresh("k");
        val y = fresh("y");
        val p = fresh("p");

        val term = new CoomaLetVTermNode(p, new CoomaPrimitiveValue(primitive, new String[]{y}),
                CoomaAppCTermNodeGen.create(k, p));

        //We need this current node to adopt the new AST nodes created in term
        super.insert(term);

        return new RowRuntimeValue(
                new FieldValueRuntime[]{
                        new FieldValueRuntime(field, new FunctionClosure(new Rho(), k, y, term))
                });
    }


    @Override
    public String getShow() {
        return String.format("cap %s", cap);
    }

}
