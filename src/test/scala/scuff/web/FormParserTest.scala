package scuff.web

import org.junit._
import org.junit.Assert._

class FormParserTest {
  @Test
  def simple {
    object Parser extends FormParser[Beano]
    val form = Map('age -> Seq("36"), 'perhaps -> Seq("false"))
    Parser(form) match {
      case Left(errors) ⇒ fail("Should have no errors, but does: " + errors)
      case Right(bean) ⇒
        assertEquals(36, bean.age)
        assertEquals(None, bean.name)
    }
  }

  @Test
  def twoStep {
    object Parser extends FormParser[Beano2] {
      private def setGeoPoint(b: Beano2) = {
        b.geoPoint = new scuff.GeoPoint(b.lat, b.lng)
        None
      }
      override val secondPasses = Map('geoPoint -> setGeoPoint _)
    }
    val form = Map('lat -> Seq("181"), 'lng -> Seq("-45.1234"))
    Parser(form) match {
      case Left(errors) ⇒ assertTrue(errors.contains('geoPoint))
      case Right(bean) ⇒ fail("Should have errors")
    }
  }
}

@reflect.BeanInfo
class Beano {
  import java.net.URL
  var name: Option[String] = _
  
  var url: Option[URL] = _
  def url(s: String) = new URL(s)
  
  var age: Int = _
  def age(s: String) = s.toInt
  
  var year: Option[Int] = _
  def year(s: String) = s.toInt
  
  var maybe: Option[Boolean] = _
  def maybe(s: String) = s.toBoolean
  
  var perhaps: Boolean = _
  def perhaps(s: String) = s.toBoolean
}

@reflect.BeanInfo
class Beano2 {
  var lat: Float = _
  def lat(s: String) = s.toFloat
  var lng: Float = _
  def lng(s: String) = s.toFloat
  private[web] var geoPoint: scuff.GeoPoint = _
}
