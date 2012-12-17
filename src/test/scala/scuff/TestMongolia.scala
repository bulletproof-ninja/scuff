package scuff

import org.junit._
import org.junit.Assert._
import org.bson.types._
import com.mongodb._

class TestMongolia {
  import Mongolia._
  @Test
  def `bson object` {
    val oid = new ObjectId("506c53b0a025ec577423ef92")
    val timestamp = new scuff.Timestamp(1349276592614L)
    val uuid = java.util.UUID.fromString("650c1d1c-3a1d-479c-a3fd-9c707e9288c4")
    val opt: Option[Int] = None
    val list = List("a", "b")
    val doc = obj(
      "nothing" := null,
      "none" := opt,
      "list" := list,
      "now" := timestamp,
      "id" := oid,
      "array" := Array("one", "two", "three"),
      "uuid" := uuid,
      "bytes" := Array[Byte](5, 23, 46, 45, 2, 23, -4, -53))
    doc.put("foo", List("abc", "def", "ghi"))
    assertEquals(doc.toString, doc.serialize)
    assertEquals("""{"nothing":null,"none":null,"list":["a","b"],"now":{"$date":1349276592614},"id":{"$oid":"506c53b0a025ec577423ef92"},"array":["one","two","three"],"uuid":{"$uuid":"650c1d1c-3a1d-479c-a3fd-9c707e9288c4"},"bytes":{ "$binary" : "BRcuLQIX/Ms=" , "$type" : 0},"foo":["abc","def","ghi"]}""", doc.serialize)
  }

  @Test
  def dupes {
    try {
      val dbo = obj("foo" := 1, "bar" := 2, "foo" := 3)
      fail("Should not silently overwrite field")
    } catch {
      case e: Exception ⇒ assertTrue(e.getMessage.indexOf("\"foo\"") > -1)
    }
  }

  @Test
  def uuid {
    val uuid = java.util.UUID.fromString("650c1d1c-3a1d-479c-a3fd-9c707e9288c4")
    val doc = obj("newUUID" := uuid)
    doc.put("oldUUID", uuid)
    assertEquals("""{"newUUID":{"$uuid":"650c1d1c-3a1d-479c-a3fd-9c707e9288c4"},"oldUUID":{"$uuid":"650c1d1c-3a1d-479c-a3fd-9c707e9288c4"}}""", doc.serialize)
    assertEquals(uuid, doc("newUUID").as[java.util.UUID])
    assertEquals(uuid, doc("oldUUID").as[java.util.UUID])
  }

  @Test
  def unset {
    val obj = $unset("foo", "bar")
    assertEquals("""{"$unset":{"foo":1,"bar":1}}""", obj.serialize)
  }

  @Test
  def increment {
    val obj = $inc("age" := 45L, "days" := -34.34)
    assertEquals("""{"$inc":{"age":45,"days":-34.34}}""", obj.serialize)
  }

  @Test
  def pushAll {
    val obj = $pushAll("bar" := arr(2, 3, 4))
    assertEquals("""{"$pushAll":{"bar":[2,3,4]}}""", obj.serialize)
  }

  @Test
  def `implicit list` {
    val dboList: Seq[DBObject] = Seq(
      obj("foo" := 5), obj("bar" := 89))
    assertEquals("""[{"foo":5},{"bar":89}]""", dboList.serialize)
  }

  @Test
  def option {
      def toObj(tuple: Option[(Int, Int)]): Option[DBObject] = tuple.map { case (first, second) ⇒ obj("first" := first, "second" := second) }
    val dbo = obj(
      "foo" := "bar",
      "tuple" := toObj(Some(4 -> 9)))
    assertEquals("""{"foo":"bar","tuple":{"first":4,"second":9}}""", dbo.serialize)
    val tuple = toObj(None)
    val dbo2 = obj(
      "foo" := "bar",
      "tuple" := tuple)
    assertEquals("""{"foo":"bar","tuple":null}""", dbo2.serialize)
  }

  @Test
  def oid {
    val dbo = obj("_id" := new ObjectId("506c53b0a025ec577423ef92"))
    assertEquals("""{"_id":{"$oid":"506c53b0a025ec577423ef92"}}""", dbo.serialize)
    val other: DBObject = dbo("_id").as[ObjectId]
    assertEquals(dbo.serialize, other.serialize)
  }

  @Test
  def in {
    val names = Array("foo", "bar")
    val filter = obj(
      $and(
        "key.day" := $gte(20120101),
        "key.day" := $lte(20121231)))
    filter.add("key.name" := $in(names: _*))
    assertEquals("""{"$and":[{"key.day":{"$gte":20120101}},{"key.day":{"$lte":20121231}}],"key.name":{"$in":["foo","bar"]}}""", filter.serialize)
  }

  @Test
  def each {
    val newNames = List("Foo", "Bar")
    val update = $addToSet("names" := $each(newNames: _*))
    assertEquals("""{"$addToSet":{"names":{"$each":["Foo","Bar"]}}}""", update.serialize)
  }

  @Test
  def nullFilter {
    val dbo = obj("foo" := null).ignoreNulls(true)
    dbo.add("bar" := null)
    assertEquals(None, dbo("foo").opt[String])
    assertEquals(None, dbo("bar").opt[String])
    dbo("foo") match {
      case f: Null ⇒ assertTrue(true)
      case _ ⇒ fail("Field should be null")
    }
    dbo("bar") match {
      case f: Missing ⇒ assertTrue(true)
      case _ ⇒ fail("Field should be missing")
    }
    val doc = obj(ignoreNulls = true).add("foo" := 5)
    assertEquals(5, doc("foo").as[Int])
    doc.add("foo" := null)
    assertEquals(None, doc("foo").opt[Int])
    assertEquals("{}", doc.serialize)
  }

  @Test
  def listAsDBO {
    val dbo: DBObject = arr("A", "B", "C")
    assertEquals("""["A","B","C"]""", dbo.serialize)
    val list = dbo.asSeq[String]
    assertEquals(Seq("A", "B", "C"), list)
  }

  @Test
  def chars {
    val foo = obj("a" := 'A', "b" := 'B', "c" := 'C')
    assertEquals('A', foo("a").as[Char])
    assertEquals('B', foo("b").as[Char])
    assertEquals('C', foo("c").as[Char])
    val bar = obj("d" := "D", "e" := "E", "f" := "F")
    assertEquals('D', bar("d").as[Char])
    assertEquals('E', bar("e").as[Char])
    assertEquals('F', bar("f").as[Char])
  }

  @Test
  def prop {
    val foo = obj("foo" := null, "bar" := 42)
    val fooProp = foo.prop("foo")
    assertEquals("foo", fooProp.key)
    assertNull(fooProp.raw)
    val barProp = foo.prop("bar")
    assertEquals("bar", barProp.key)
    assertEquals(42, barProp.raw)
    val bar = obj(barProp)
    assertEquals(42L, bar("bar").as[Long])
  }

  @Test
  def addMap {
    val map = Map("foo" -> 40, "bar" -> 80)
    val dbo = obj("baz" := 33)
    dbo.add(map)
    assertEquals(40, dbo("foo").as[Int])
    assertEquals(80, dbo("bar").as[Int])
    assertEquals(33, dbo("baz").as[Int])
    val fromMap = obj(map)
    assertEquals(40, fromMap("foo").as[Int])
  }

//  @Test
//  def mapReduceCoffee {
//    val coll = new RichDBCollection(null)
//    val map = "-> emit(@days[0].toString().substring(0,4), count: 1); return"
//    val reduce = "(key, values) -> count: values.reduce (t, v) -> t + v.count"
//    val res = coll.mapReduce(map)(reduce)(obj("trend" := 234))
//    val res2 = coll.mapReduce(map)(reduce)()
//  }

}