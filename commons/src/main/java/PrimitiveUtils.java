import java.io.IOException;
import java.io.Reader;

public class PrimitiveUtils {

	public static String readReaderContents(Reader in) throws IOException{
		try (Reader intry = in) {
			StringBuilder sb = new StringBuilder();
			int c;
			while ((c = intry.read()) != -1) {
				sb.append(((char) c));
			}
			return sb.toString();
		}
	}
}
