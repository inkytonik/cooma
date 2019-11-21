package org.bitbucket.inkytonik.cooma;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

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

	public static boolean isFileWritable(String filePath){
		return Files.isWritable(Paths.get(filePath));
	}

	public static boolean isFileReadable(String filePath){
		return Files.isReadable(Paths.get(filePath));
	}

}
