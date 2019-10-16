package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.Backend;
import org.bitbucket.inkytonik.cooma.Primitives;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FunctionClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RecRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import scala.collection.JavaConverters;

import java.util.Arrays;

@Getter
@NodeInfo(shortName = "prmV", description = "Primitive value")
public class CoomaPrimitiveValue extends CoomaValueNode {

    private final String[] xs;
    private final Primitives.Primitive p;
    private final Backend backend;

    public CoomaPrimitiveValue(Backend backend, Primitives.Primitive p, String[] xs) {
        this.p = p;
        this.xs = xs;
        this.backend = backend;
    }

    @Override
    public RuntimeValue evaluate(VirtualFrame frame) {
        RuntimeValue primitive = (RuntimeValue) p.eval(backend, obtainRho(),
                JavaConverters.asScalaIteratorConverter(Arrays.asList(xs).iterator()).asScala().toVector(),
                JavaConverters.asScalaIteratorConverter(Arrays.asList(getArgs()).iterator()).asScala().toVector());

        if (primitive instanceof RecRuntimeValue){
            Arrays.stream(((RecRuntimeValue) primitive).getFields())
                    .filter( field -> field.getV() instanceof FunctionClosure)
                    .filter( field -> ((FunctionClosure) field.getV()).getZ().getParent() == null )
                    .forEach( field -> this.insert(( (FunctionClosure) field.getV()).getZ()));
        }

        return primitive;
    }
}
