package org.bitbucket.inkytonik.cooma;

import java.util.stream.Collectors;

public class Utils {

    public static String escape(String s) {
        return s.chars().mapToObj(i -> (char) i).map(Utils::escapedChar).collect(Collectors.joining());
    }

    private static String escapedChar(char c) {
        switch (c) {
            case '\b':
                return "\\b";
            case '\t':
                return "\\t";
            case '\n':
                return "\\n";
            case '\f':
                return "\\f";
            case '\r':
                return "\\r";
            case '"':
                return "\\\"";
            case '\'':
                return "\\\'";
            case '\\':
                return "\\\\";
            default:
                return (Character.isISOControl(c)) ? "\\" + Integer.toOctalString(c) : String.valueOf(c);
        }
    }
}
