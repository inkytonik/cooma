/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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
