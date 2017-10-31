package scuff

import org.junit._
import org.junit.Assert._
import java.util.UUID
import java.util.Arrays
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

  val secretKey = Hmac.generateKey(56, "DES")

  val HmacUserCodec = Hmac(Codec.UTF8(UserJsonCodec), secretKey)
  val Base64HmacUser = Base64(HmacUserCodec)
  val JsonHmac = Hmac.json(UserJsonCodec, new HmacFunction(secretKey))

  private def twoWay[T](user: User, hmac: Codec[User, T]) {
    val encoded = hmac.encode(user)
    val decoded = hmac.decode(encoded)
    assertEquals(user, decoded)
  }
  private def twoWayModified[T](shouldFail: Boolean)(user: User, hmac: Codec[User, T])(modifier: T => T) {
    val encoded = hmac.encode(user)
    val modified = modifier(encoded)
    try {
      hmac.decode(modified)
      if (shouldFail) fail("Should fail on modified data")
    } catch {
      case _: SignatureException if shouldFail => // As expected
    }
  }

  @Test
  def `two way`() {
    val user = new User(System.currentTimeMillis + 60000, UUID.randomUUID)
    twoWay(user, Base64HmacUser)
    twoWay(user, JsonHmac)
  }

  @Test
  def `two way, modified`() {
    val exp = System.currentTimeMillis + 60000
    val user = new User(exp, UUID.randomUUID)
    twoWayModified(shouldFail = true)(user, HmacUserCodec) { encoded =>
      val len = Numbers.bytesToInt(encoded)
      val jsonUser = Arrays.copyOfRange(encoded, 4, 4 + len).utf8
      val modifiedJson = jsonUser.replace(exp.toString, (exp + 123456).toString)
      val modifiedJsonBytes = modifiedJson.utf8
      System.arraycopy(modifiedJsonBytes, 0, encoded, 4, len)
      encoded
    }
    twoWayModified(shouldFail = true)(user, JsonHmac) { encoded =>
      encoded.replace(user.expiration.toString, (user.expiration+1).toString)
    }
    twoWayModified(shouldFail = false)(user, JsonHmac) { encoded =>
      val ast = scuff.JsonParserPool.use(_ parseMap encoded)
      assertEquals(2, ast.size)
      val hash = ast.get("hash")
      ast.get("data") match {
        case data: java.util.Map[_, _] =>
          assertEquals(user.id.toString, data.get("id"))
          assertEquals(Long box user.expiration, data.get("expiration"))
      }
      val userJson = UserJsonCodec.encode(user)
      s"""{"hash":"$hash","data":$userJson}"""
    }
  }

}
