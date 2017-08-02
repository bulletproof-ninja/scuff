package scuff

import javax.management.MXBean
import scala.beans.BeanProperty

import org.junit._, Assert._
import javax.management.ObjectName

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

  private def uniqueNames(quoted: Boolean, beanName: String, typeName: String, i1: AnyRef, i2: AnyRef) {
    val objName1 = JMX.register(i1, beanName)
    if (quoted) assertEquals(ObjectName quote beanName, objName1.getKeyProperty("name"))
    else assertEquals(beanName, objName1.getKeyProperty("name"))
    val objName2 = JMX.register(i2, beanName)
    if (quoted) assertEquals(ObjectName quote s"$beanName[1]", objName2.getKeyProperty("name"))
    else assertEquals(s"$beanName[1]", objName2.getKeyProperty("name"))

    assertEquals(typeName, objName1.getKeyProperty("type"))
    assertEquals(typeName, objName2.getKeyProperty("type"))
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
  def quoteUnquoted() {
    val myBean = FooBeanCaseClassImpl(99)
    val objName = JMX.register(myBean, "My Bean")
    assertEquals("\"My Bean\"", objName.getKeyProperty("name"))
  }

}
