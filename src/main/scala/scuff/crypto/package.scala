package scuff

import scala.util.Try
import java.security.Security
import java.security.Provider

package object crypto {

  val SecureRandom = java.security.SecureRandom.getInstanceStrong

  Try(Class.forName("org.bouncycastle.jce.provider.BouncyCastleProvider")).foreach { bcProv =>
    Security addProvider bcProv.getDeclaredConstructor().newInstance().asInstanceOf[Provider]
  }
}
