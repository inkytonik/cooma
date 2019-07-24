package org.bitbucket.inkytonik.cooma.truffle;

import com.oracle.truffle.api.TruffleFile;

import java.nio.charset.Charset;

public class CoomaFileDetector implements TruffleFile.FileTypeDetector {

    @Override
    public String findMimeType(TruffleFile file) {
        String name = file.getName();
        if (name != null && name.endsWith(".cooma")) {
            return CoomaConstants.MIME_TYPE;
        }
        return null;
    }

    @Override
    public Charset findEncoding(TruffleFile file) {
        return null;
    }
}
