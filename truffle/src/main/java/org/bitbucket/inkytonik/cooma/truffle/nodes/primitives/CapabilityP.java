package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import lombok.RequiredArgsConstructor;
import lombok.val;
import org.bitbucket.inkytonik.cooma.truffle.exceptions.CapabilityException;
import org.bitbucket.inkytonik.cooma.truffle.exceptions.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Pair;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaAppCTermNodeGen;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaLetVTermNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaPrimitiveValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.*;

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
				try {
					result = makeCapability(Collections.singletonList(
							new Pair<>("write", new WriterWriteP(argument))));
				} catch (CapabilityException e) {
					result = new ErrorRuntimeValue(e.getMessage());
				}
				break;

			case "Reader":
				try {
					result = makeCapability(Collections.singletonList(
							new Pair<>("read", new ReaderReadP(argument))));
				} catch (CapabilityException e) {
					result = new ErrorRuntimeValue(e.getMessage());
				}
				break;

			case "ReaderWriter":
				try {
					result = makeCapability(Arrays.asList(
							new Pair<>("write", new WriterWriteP(argument)),
							new Pair<>("read", new ReaderReadP(argument))));
				} catch (CapabilityException e) {
					result = new ErrorRuntimeValue(String.format("ReaderWriter capability unavailable. %s", e.getMessage()));
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
