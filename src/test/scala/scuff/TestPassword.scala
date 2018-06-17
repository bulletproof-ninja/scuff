package scuff

import org.junit._
import org.junit.Assert._

class TestPassword {
  import language.implicitConversions
  implicit def config(algo: String) = new Password.Config(algo, 0, 1)
  implicit def config(salt: Int) = new Password.Config("SHA-256", salt, 1)
  implicit val config: Password.Config = config("SHA-256")

  @Test def equality(): Unit = {
    val str = "pAssWord9"
    val pwd = Password(str)("sha")
    val p2 = Password(str)("SHA")
    assertEquals(pwd, p2)
    assertFalse(p2.equals(str))
    assertFalse(p2.equals(p2.toString))
  }

  @Test def matching(): Unit = {
    val str = "pAssWord9"
    val pwd = Password(str)("sha")
    assertTrue(pwd.matches(str))
    assertEquals(java.security.MessageDigest.getInstance("SHA").getDigestLength, pwd.digest.length)
  }

  @Test def digestLength(): Unit = {
    val str = "pAssWord9"
    val sha = Password(str)("sha")
    assertEquals(java.security.MessageDigest.getInstance("SHA").getDigestLength, sha.digest.length)
    val sha512 = Password(str)("SHA-512")
    assertEquals(java.security.MessageDigest.getInstance(sha512.algorithm).getDigestLength, sha512.digest.length)
  }

  @Test def defaultAlgo(): Unit = {
    val str = "spazword"
    val pwd = Password(str)
    assertEquals(java.security.MessageDigest.getInstance(pwd.algorithm).getDigestLength, pwd.digest.length)
  }

  @Test def salty(): Unit = {
    val str = "パスワードは"
    val pwd = Password(str)(7)
    assertTrue(pwd.matches(str))
  }

  @Test def slowButSafe(): Unit = {
    import scala.concurrent.duration._
    val config = new Password.Config("SHA-256", 7, 133.milliseconds)
    val str = "Foo and Bar went for a walk with パスワードは"
    val before = System.currentTimeMillis()
    val pwd = Password(str)(config)
    val msSpent = System.currentTimeMillis - before
    assertTrue(msSpent >= 133)
    assertTrue(pwd.matches(str))
  }
}
