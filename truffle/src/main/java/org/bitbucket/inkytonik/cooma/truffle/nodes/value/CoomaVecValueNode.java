package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VecRuntimeValue;
import scala.collection.immutable.Vector;

@Getter
@RequiredArgsConstructor
@NodeInfo(shortName = "vecV", description = "Vector value")
public class CoomaVecValueNode extends CoomaValueNode {

	private final Vector<String> vector;

	@Override
	public RuntimeValue evaluate(VirtualFrame frame) {
		return new VecRuntimeValue(vector.map(this::obtainFromRho));
	}
}
