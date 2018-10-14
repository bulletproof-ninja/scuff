package scuff

import org.junit._, Assert._

import java.io._

class TestMediaType {
  @Test
  def `remove parm`(): Unit = {
    val mt = MediaType("application/json;encoding=UTF-8")
    assertEquals("UTF-8", mt.parm("encoding").get)
    val mt2 = mt.removeParm("encoding")
    assertEquals("UTF-8", mt.parm("encoding").get)
    assertEquals(None, mt2.parm("encoding"))
    val mt3 = mt.addParm("foo", 42)
    assertEquals("UTF-8", mt3.parm("encoding").get)
    assertEquals(42, mt3.parm("foo", _.toInt).get)
    val mt4 = mt3.removeParm("encoding")
    assertEquals(None, mt4.parm("encoding"))
    assertEquals(42, mt4.parm("foo", _.toInt).get)
  }
  @Test
  def `add parm`(): Unit = {
    val mt = MediaType("text/html")
    assertEquals(None, mt.parm("foo"))
    val mt2 = mt.addParm("foo", 42)
    assertEquals(None, mt.parm("foo"))
    assertEquals(42, mt2.parms("foo").toInt)
    val mt3 = mt2.addParm("foo", 43)
    assertEquals(None, mt.parm("foo"))
    assertEquals(42, mt2.parms("foo").toInt)
    assertEquals(43, mt3.parms("foo").toInt)
    val mt4 = mt3.addParm("bar", "Hello")
    assertEquals(None, mt.parm("bar"))
    assertEquals(None, mt2.parm("bar"))
    assertEquals(None, mt3.parm("bar"))
    assertEquals("Hello", mt4.parms("bar"))
  }

  @Test
  def tree(): Unit = {
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

  @Test
  def serialization() = {
    val mt1 = MediaType("application", "json", "encoding" -> "UTF-8")
    val baOut = new ByteArrayOutputStream
    val out = new ObjectOutputStream(baOut)
    out.writeObject(mt1)
    val arr = baOut.toByteArray()
    val baInp = new ByteArrayInputStream(arr)
    val inp = new ObjectInputStream(baInp)
    val mt2 = inp.readObject().asInstanceOf[MediaType]
    assertFalse(mt1 eq mt2)
    assertTrue(mt1 matches mt2)
    assertTrue(mt2 matches mt1)
    assertEquals(mt1.##, mt2.##)
    assertEquals(mt1, mt2)
    val mt3 = mt1.removeParm("encoding")
    assertNotEquals(mt1, mt3)
    assertTrue(mt1 matches mt3)
  }
}
