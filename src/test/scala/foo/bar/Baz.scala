package foo.bar

import javax.management.MXBean
import scala.beans.BeanProperty

@MXBean
trait Baz {
  def add(key: String): Unit
  def remove(key: String): Unit
  def getCurrValue(): Long
  def setCurrValue(v: Long): Unit
}

object Baz {
  def newBean(map: => collection.Map[String, Any]) = new Baz with scuff.JMX.DynamicMBean {
    def dynamicProps = map
    def add(key: String): Unit = ()
    def remove(key: String): Unit = ()
    @BeanProperty var currValue: Long = 0
  }
}
