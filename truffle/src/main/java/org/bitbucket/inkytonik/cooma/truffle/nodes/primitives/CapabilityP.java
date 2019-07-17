package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import lombok.RequiredArgsConstructor;
import lombok.val;
import org.bitbucket.inkytonik.cooma.truffle.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Pair;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaAppCTermNodeGen;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaLetVTermNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaPrimitiveValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.*;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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

            case "Writer":
                if (Files.isWritable(Paths.get(argument))) {
                    result = makeCapability(Collections.singletonList(
                            new Pair<>("write", new ConsoleWriteP(argument))));
                } else {
                    result = new ErrorRuntimeValue(String.format("Writer capability unavailable: can't write %s", argument));
                }
                break;

            case "Reader":
                if (Files.isReadable(Paths.get(argument))) {
                    result = makeCapability(Collections.singletonList(
                            new Pair<>("read", new ReaderReadP(argument))));
                } else {
                    result = new ErrorRuntimeValue(String.format("Reader capability unavailable: can't read %s", argument));
                }
                break;

            case "ReaderWriter":
                if (Files.isReadable(Paths.get(argument)) && Files.isWritable(Paths.get(argument))) {
                    result = makeCapability(Arrays.asList(
                            new Pair<>("write", new ConsoleWriteP(argument)),
                            new Pair<>("read", new ReaderReadP(argument))));
                } else {
                    result = new ErrorRuntimeValue(String.format("ReaderWriter capability unavailable: can't read and write %s", argument));
                }
                break;
            default:
                throw new CoomaException(String.format("capability: unknown primitive %s", name), this);
        }

        return result;
    }

    private RowRuntimeValue makeCapability(List<Pair<String, Primitive>> pairs) {
        return new RowRuntimeValue(pairs.stream().map(pair -> {
                    val k = fresh("k");
                    val y = fresh("y");
                    val p = fresh("p");

                    val term = new CoomaLetVTermNode(p, new CoomaPrimitiveValue(pair.getRight(), new String[]{y}),
                            CoomaAppCTermNodeGen.create(k, p));
                    //We need this current node to adopt the new AST nodes created in term
                    super.insert(term);
                    return new FieldValueRuntime(pair.getLeft(), new FunctionClosure(new Rho(), k, y, term));
                }
        ).toArray(FieldValueRuntime[]::new));
    }


    @Override
    public String getShow() {
        return String.format("cap %s", cap);
    }

}
