package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class LiteralTests extends ExpressionTests {

    test(
        "positive integer",
        "42",
        "42",
        "Int"
    )

    test(
        "bracketed expression",
        "{10}",
        "10",
        "Int"
    )

    test(
        "positive integer larger than 32 bits",
        "4294967296123",
        "4294967296123",
        "Int"
    )

    test(
        "positive integer larger than 64 bits",
        "123456789123456789123456789123456789",
        "123456789123456789123456789123456789",
        "Int"
    )

    test(
        "negative integer",
        "-182",
        "-182",
        "Int"
    )

    test(
        "negative integer larger than 32 bits",
        "-4294967296123",
        "-4294967296123",
        "Int"
    )

    test(
        "negative integer larger than 64 bits",
        "-123456789123456789123456789123456789",
        "-123456789123456789123456789123456789",
        "Int"
    )

    test(
        "string",
        """"hello"""",
        """"hello"""",
        "String"
    )

    test(
        "string with quote",
        """"hel\"lo"""",
        """"hel\"lo"""",
        "String"
    )

    test(
        "string with newline",
        """"hello\n"""",
        """"hello\n"""",
        "String"
    )

    test(
        "string with escape sequences",
        """"\b\f\n\r\t\\\"\'"""",
        """"\b\f\n\r\t\\\"\'"""",
        "String"
    )

}
