package scuff

import org.junit._
import org.junit.Assert._
import java.security.spec.RSAPrivateKeySpec
import javax.crypto.SecretKeyFactory
import java.util.UUID
import java.util.Arrays
import javax.crypto.spec.DESKeySpec
import javax.crypto.KeyGenerator
import java.security.SignatureException

class TestHmac {

  case class User(expiration: Long, id: UUID)

  object UserJsonCodec extends Codec[User, String] {
    val getExp = """
"expiration":(\d+)
""".trim.r
    val getId = """
"id":"([\w-]+)"
""".trim.r
    def encode(user: User): String = s"""{"id":"${user.id}","expiration":${user.expiration}}"""
    def decode(json: String): User = {
      val exp = getExp.findFirstMatchIn(json).flatMap(m => Option(m.group(1))).map(_.toLong).getOrElse(sys.error("bah"))
      val id = getId.findFirstMatchIn(json).flatMap(m => Option(m.group(1))).map(UUID.fromString).getOrElse(sys.error("bah"))
      new User(exp, id)
    }

  }

  val secretKey = KeyGenerator.getInstance("DES").generateKey

  val HmacUserCodec = new Hmac(Codec.BinaryUTF8(UserJsonCodec), secretKey)
  val Base64HmacUser = Base64(HmacUserCodec)

  @Test
  def `two way` {
    val user = new User(System.currentTimeMillis + 60000, UUID.randomUUID)
    val encoded = Base64HmacUser.encode(user)
    val decoded = Base64HmacUser.decode(encoded)
    assertEquals(user, decoded)
  }

  @Test
  def `two way, modified` {
    val exp = System.currentTimeMillis + 60000
    val user = new User(exp, UUID.randomUUID)
    val encoded = HmacUserCodec.encode(user)
    val len = Numbers.bytesToInt(encoded)
    val jsonUser = Arrays.copyOfRange(encoded, 4, 4 + len).utf8
    val modifiedJson = jsonUser.replace(exp.toString, (exp + 123456).toString)
    val modifiedJsonBytes = modifiedJson.utf8
    System.arraycopy(modifiedJsonBytes, 0, encoded, 4, len)
    try {
      HmacUserCodec.decode(encoded)
      fail("Should fail on modified data")
    } catch {
      case _: SignatureException => // As expected
    }
  }

}
