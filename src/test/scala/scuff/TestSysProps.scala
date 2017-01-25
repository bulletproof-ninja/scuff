package scuff

import org.junit._
import org.junit.Assert._

class TestSysProps {
  @Test(expected = classOf[IllegalStateException])
  def `invalid value` {
    System.setProperty("foo", "baz")
    SysProps.optional("foo", Set("foo", "bar"))
    fail("Should have failed on 'baz'")
  }

  @Test
  def `valid value` {
    System.setProperty("foo", "baz")
    assertEquals("baz", SysProps.required("foo", Set("foo", "bar", "baz")))
  }

  @Test
  def `error message` {
    System.setProperty("foo", "baz")
    try {
    SysProps.required("foo", Set("foo", "bar"))
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }

  @Test
  def `env fallback` {
    val props = new SysProps(EnvVars)
    System.getenv("JAVA_HOME") match {
      case null => assertEquals(None, props.optional("JAVA_HOME"))
      case javaHome => assertEquals(javaHome, props.required("JAVA_HOME"))
    }
  }

}
