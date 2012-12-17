package scuff

import org.junit._
import org.junit.Assert._

class TestPassword {

  @Test def equality() {
    val str = "pAssWord9"
    val pwd = Password(str, "sha")
    val p2 = Password(str, "SHA")
    assertEquals(pwd, p2)
    assertFalse(p2.equals(str))
    assertFalse(p2.equals(p2.toString))
  }

  @Test def matching() {
    val str = "pAssWord9"
    val pwd = Password(str, "sha")
    assertTrue(pwd.matches(str))
    assertEquals(java.security.MessageDigest.getInstance("SHA").getDigestLength, pwd.digest.length)
  }

  @Test def digestLength {
    val str = "pAssWord9"
    val sha = Password(str, "sha")
    assertEquals(java.security.MessageDigest.getInstance("SHA").getDigestLength, sha.digest.length)
    val sha512 = Password(str, "SHA-512")
    assertEquals(java.security.MessageDigest.getInstance(sha512.algorithm).getDigestLength, sha512.digest.length)
  }

  @Test def defaultAlgo {
    val str = "spazword"
    val pwd = Password(str)
    assertEquals(java.security.MessageDigest.getInstance(pwd.algorithm).getDigestLength, pwd.digest.length)
  }

  @Test def salty {
    val str = "パスワードは"
    val salt = Password.randomSalt(7)
    val pwd = Password(str, salt)
    assertTrue(pwd.matches(str))
  }
}