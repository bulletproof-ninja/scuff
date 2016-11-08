package scuff

import org.junit._, Assert._

class TestMediaType {
  @Test
  def `remove parm` {
    val mt = MediaType("application/json;encoding=UTF-8")
    assertEquals("UTF-8", mt.parm("encoding").get)
    val mt2 = mt.removeParm("encoding")
    assertEquals("UTF-8", mt.parm("encoding").get)
    assertEquals(None, mt2.parm("encoding"))
    val mt3 = mt.addParm("foo", 42)
    assertEquals("UTF-8", mt3.parm("encoding").get)
    assertEquals(42, mt3.parm("foo").get.toInt)
    val mt4 = mt3.removeParm("encoding")
    assertEquals(None, mt4.parm("encoding"))
    assertEquals(42, mt4.parm("foo").get.toInt)
  }
  @Test
  def `add parm` {
    val mt = MediaType("text/html")
    assertEquals(None, mt.parm("foo"))
    val mt2 = mt.addParm("foo", 42)
    assertEquals(None, mt.parm("foo"))
    assertEquals(42, mt2.parm("foo").get.toInt)
    val mt3 = mt2.addParm("foo", 43)
    assertEquals(None, mt.parm("foo"))
    assertEquals(42, mt2.parm("foo").get.toInt)
    assertEquals(43, mt3.parm("foo").get.toInt)
    val mt4 = mt3.addParm("bar", "Hello")
    assertEquals(None, mt.parm("bar"))
    assertEquals(None, mt2.parm("bar"))
    assertEquals(None, mt3.parm("bar"))
    assertEquals("Hello", mt4.parm("bar").get)
  }
}
