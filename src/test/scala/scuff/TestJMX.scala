package scuff

import javax.management.MXBean
import scala.beans.BeanProperty

import org.junit._, Assert._
import javax.management.ObjectName
import java.net.BindException

object TestJMX {
  trait FooBeanMXBean {
    def getFoo(): Int
  }
  @MXBean
  trait BarBean {
    def getBar(): String
  }
  case class FooBeanCaseClassImpl(@BeanProperty foo: Int) extends FooBeanMXBean
  case class BarBeanImpl(var bar: String) extends BarBean {
    def getBar = bar
  }
}
class TestJMX {
  import TestJMX._

  @Test @Ignore
  def dynamic(): Unit = {
    var map = Map("a.b.c" -> 0, "d.e.f" -> 0, "g.h.i" -> 0)
    val bean = foo.bar.Baz.newBean(map)
      def updateValues(): Unit = {
        map = map.keys.foldLeft(Map.empty[String, Int]) {
          case (map, key) => map.updated(key, util.Random.nextInt(100))
        }
      }
    updateValues()
    val reg = JMX.register(bean, "random_values")
    while (true) {
      Thread sleep 5000
      updateValues()
    }
    reg.cancel()
  }

  private def uniqueNames(quoted: Boolean, beanName: String, typeName: String, i1: AnyRef, i2: AnyRef): Unit = {
    val reg1 = JMX.register(i1, beanName)
    if (quoted) assertEquals(ObjectName quote beanName, reg1.name.getKeyProperty("name"))
    else assertEquals(beanName, reg1.name.getKeyProperty("name"))
    val reg2 = JMX.register(i2, beanName)
    if (quoted) assertEquals(ObjectName quote s"$beanName[1]", reg2.name.getKeyProperty("name"))
    else assertEquals(s"$beanName[1]", reg2.name.getKeyProperty("name"))

    assertEquals(typeName, reg2.name.getKeyProperty("type"))
    assertEquals(typeName, reg2.name.getKeyProperty("type"))
    (reg1 :: reg2 :: Nil).foreach(_.cancel)
  }

  @Test
  def uniqueNames_foo() = {
    uniqueNames(false, "MyFoo", "FooBean", FooBeanCaseClassImpl(5), FooBeanCaseClassImpl(7))
    uniqueNames(true, "My Foo", "FooBean", FooBeanCaseClassImpl(55), FooBeanCaseClassImpl(77))
  }
  @Test
  def uniqueNames_bar() = {
    uniqueNames(false, "MyBar", "BarBean", new BarBeanImpl("5"), BarBeanImpl("7"))
    uniqueNames(true, "My,Bar", "BarBean", new BarBeanImpl("55"), BarBeanImpl("77"))
  }

  @Test
  def quoteUnquoted(): Unit = {
    val myBean = FooBeanCaseClassImpl(99)
    val reg = JMX.register(myBean, "My Bean")
    assertEquals("\"My Bean\"", reg.name.getKeyProperty("name"))
    reg.cancel()
  }

  @Test
  def descriptiveBindException(): Unit = {
    val server = JMX.startJMXMP()
    val port = server.getAddress.getPort
    try {
      JMX.startJMXMP(port)
    } catch {
      case be: BindException =>
        be.printStackTrace()
        assertTrue(be.getMessage contains s":$port")
        assertTrue(be.getCause.isInstanceOf[BindException])
    } finally {
      server.stop()
    }
  }

}
