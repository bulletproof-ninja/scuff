package scuff

import org.junit._
import org.junit.Assert._
import java.io.File
import java.nio.charset.Charset

class TestProps {
  @Test(expected = classOf[IllegalStateException])
  def `invalid value`(): Unit = {
    System.setProperty("foo", "baz")
    SysProps.optional("foo", Set("foo", "bar"))
    fail("Should have failed on 'baz'")
  }

  @Test
  def `valid value`(): Unit = {
    System.setProperty("foo", "baz")
    assertEquals("baz", SysProps.required("foo", Set("foo", "bar", "baz")))
  }

  @Test
  def `error message`(): Unit = {
    System.setProperty("foo", "baz")
    try {
      SysProps.required("foo", Set("foo", "bar"))
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }

  @Test
  def `env fallback`(): Unit = {
    val props = new SysProps(EnvVars)
    System.getenv("JAVA_HOME") match {
      case null => assertEquals(None, props.optional("JAVA_HOME"))
      case javaHome =>
        assertEquals(javaHome, props.required("JAVA_HOME"))
        try {
          props.required("JAVA_HOME", Set("hahaha"))
          fail("Should fail because not funny")
        } catch {
          case e: IllegalStateException =>
            assertTrue(e.getMessage.contains("environment variable"))
        }
    }
  }

  @Test
  def from_file(): Unit = {
    val file = new File(getClass.getResource("/FooBar.properties").getFile)
    println(file.getAbsolutePath)
    val props = Props(file)
    assertEquals("Hello", props.required("say"))
    try {
      props.required("blablabla")
    } catch {
      case e: IllegalStateException =>
        println(e.getMessage)
        assertTrue(e.getMessage.contains(file.getName))
    }
  }

  @Test
  def typed_values(): Unit = {
    val FileEncoding = SysProps.Key("file.encoding", Charset.forName)
    val ArchDataModel = Props.Key("sun.arch.data.model")(_.toInt)
    SysProps.optional(FileEncoding).foreach { enc =>
      assertEquals(Charset.forName(enc.name), enc)
    }
    SysProps.optional(ArchDataModel).foreach { dm =>
      assertEquals(0, dm % 8)
    }
    val Number = Props.Key("number")(_.toInt)
    val testProps = Props("test props", "number" -> "234b23")
    try { testProps.required(Number); fail("Should fail on invalid number") } catch {
      case arg: IllegalArgumentException =>
        assertTrue(arg.getMessage contains "\"number\"")
        assertTrue(arg.getMessage contains "\"234b23\"")
        assertTrue(arg.getMessage contains "int")
    }
  }

}
