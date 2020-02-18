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

@Getter
@RequiredArgsConstructor
@NodeInfo(shortName = "vecV", description = "Vector value")
public class CoomaVecValueNode extends CoomaValueNode {

	private final Vector<String> vector;

	@Override
	public RuntimeValue evaluate(VirtualFrame frame) {
		Vector<RuntimeValue> values = vector.map(this::obtainFromRho);
		Option<RuntimeValue> error = values.find( r -> r instanceof ErrorRuntimeValue);
		if (error.isDefined()){
			return error.get();
		}
		return new VecRuntimeValue(values);
	}
}
