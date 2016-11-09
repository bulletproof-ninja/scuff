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

  @Test
  def tree {
    val html = MediaType("text/html")
    assertEquals(None, html.treeType)
    val xhtml = MediaType("text/xhtml+xml;charset=utf8")
    assertEquals(None, xhtml.treeType.get.prefix)
    assertEquals("xhtml", xhtml.treeType.get.typeName)
    assertEquals("xml", xhtml.treeType.get.suffixType)
    val vendor = MediaType("application/vnd.scuff.foo+json;charset=utf8")
    assertEquals("vnd", vendor.treeType.get.prefix.get)
    assertEquals("scuff.foo", vendor.treeType.get.typeName)
    assertEquals("json", vendor.treeType.get.suffixType)
    assertTrue(MediaType("application/*").matches(vendor))
    assertFalse(MediaType("application/json").matches(vendor))
    assertTrue(MediaType("application/json").matches(vendor.pruned))
  }
}