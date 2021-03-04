package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ErrorRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VecRuntimeValue;
import scala.Option;
import scala.collection.immutable.Vector;
import scala.collection.immutable.VectorBuilder;
import scala.collection.Iterator;

@Getter
@RequiredArgsConstructor
@NodeInfo(shortName = "vecV", description = "Vector value")
public class CoomaVecValueNode extends CoomaValueNode {

	private final Vector<String> vector;

	@Override
	public RuntimeValue evaluate(VirtualFrame frame) {
		VectorBuilder<RuntimeValue> values = new VectorBuilder<RuntimeValue>();
		Iterator<String> iter = vector.iterator();
		while (iter.hasNext()) {
			RuntimeValue value = obtainFromRho(iter.next());
			if (value instanceof ErrorRuntimeValue)
				return value;
			values.addOne(value);
		}
		return new VecRuntimeValue(values.result());
	}

}
