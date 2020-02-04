package org.bitbucket.inkytonik.cooma;

import org.bitbucket.inkytonik.cooma.truffle.TruffleFrontend;
import java.util.Arrays;
import static scala.collection.JavaConverters.collectionAsScalaIterableConverter;

public class CoomaIRTruffleMain {
	public static void main(String[] args) {
		Config config = new Config(collectionAsScalaIterableConverter(Arrays.asList(args)).asScala().toSeq());
		config.verify();
		new TruffleFrontend(System.in, System.out).interpret(config);
	}
}
