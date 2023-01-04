package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class OperatorTests extends SemanticTests {

  // Ill-typed operator usage. Only desguaring checks are tested here.
  // Other errors are tested in their desugared form.

  test(
    "bad absolute value/length operator",
    "||true|",
    """|1:1:error: illegal |<< False : Unit, True : Unit >>|, only Int, String or Vector supported
           ||true|
           |^
           |"""
  )

  test(
    "bad concatenation operator",
    """1 ++ "hi"""",
    """|1:1:error: illegal Int ++ String, only String or Vector supported
           |1 ++ "hi"
           |^
           |"""
  )

  test(
    "bad index operator",
    """"hi"!2""",
    """|1:1:error: illegal String!, only Vector supported
           |"hi"!2
           |^
           |"""
  )

  test(
    "relational operator is not associative",
    "1 < 2 < 3",
    """|1:7:error: exp op expected
           |1 < 2 < 3
           |      ^
           |"""
  )

}
