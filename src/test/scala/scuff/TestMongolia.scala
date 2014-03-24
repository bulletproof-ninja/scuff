package scuff

import org.junit._
import org.junit.Assert._
import org.bson.types._
import com.mongodb._
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import scala.reflect.ClassTag
import java.util.Arrays
import java.util.Locale

class TestMongolia {
  import Mongolia._
  @Test
  def `bson object` {
    val oid = new ObjectId("506c53b0a025ec577423ef92")
    val timestamp = new Timestamp(1349276592614L)
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
      "emptyList" := List[Int](),
      "bytes" := Array[Byte](5, 23, 46, 45, 2, 23, -4, -53))
    doc.put("foo", List("abc", "def", "ghi"))
    assertEquals("""{"nothing":null,"none":null,"list":["a","b"],"now":{"$date":1349276592614},"id":{"$oid":"506c53b0a025ec577423ef92"},"array":["one","two","three"],"uuid":{"$uuid":"650c1d1c-3a1d-479c-a3fd-9c707e9288c4"},"emptyList":[],"bytes":{ "$binary" : "BRcuLQIX/Ms=" , "$type" : 0},"foo":["abc","def","ghi"]}""", doc.toJson)
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
    assertEquals("""{"newUUID":{"$uuid":"650c1d1c-3a1d-479c-a3fd-9c707e9288c4"},"oldUUID":{"$uuid":"650c1d1c-3a1d-479c-a3fd-9c707e9288c4"}}""", doc.toJson)
    assertEquals(uuid, doc("newUUID").as[java.util.UUID])
    assertEquals(uuid, doc("oldUUID").as[java.util.UUID])
    val bytes = UUIDCdc.encode(uuid).raw.asInstanceOf[Binary]
    val binary4 = new com.mongodb.util.Base64Codec().encode(bytes.getData)
    assertEquals("ZQwdHDodR5yj/ZxwfpKIxA==", binary4)
  }

  @Test
  def unset {
    val obj = $unset("foo", "bar")
    assertEquals("""{"$unset":{"foo":1,"bar":1}}""", obj.toJson)
  }

  @Test
  def increment {
    val obj = $inc("age" := 45L, "days" := -34.34)
    assertEquals("""{"$inc":{"age":45,"days":-34.34}}""", obj.toJson)
  }

  @Test
  def geoPoint {
    val gp = GeoPoint.parse("35.027311, -111.023075", 1.23f).get
    val dbo = obj("location" := gp)
    assertEquals("""{"location":{"type":"Point","coordinates":[-111.0230712890625,35.02730941772461],"radius":1.2300000190734863}}""", dbo.toJson)
    assertEquals("""{"location":{"type":"Point","coordinates":[-111.0230712890625,35.02730941772461]}}""", obj("location" := gp.copy(radius = 0f)).toJson)
    assertEquals("""{"location":{"$near":{"$geometry":{"type":"Point","coordinates":[-111.0230712890625,35.02730941772461]},"$maxDistance":1.2300000190734863}}}""", obj("location" := $near(gp)).toJson)
    assertEquals("""{"location":{"$near":{"$geometry":{"type":"Point","coordinates":[-111.0230712890625,35.02730941772461]}}}}""", obj("location" := $near(gp.copy(radius = 0f))).toJson)
  }

  @Test
  def pushAll {
    val obj = $pushAll("bar" := arr(2, 3, 4))
    assertEquals("""{"$pushAll":{"bar":[2,3,4]}}""", obj.toJson)
  }

  @Test
  def `implicit list` {
    val dboList: Seq[DBObject] = Seq(
      obj("foo" := 5), obj("bar" := 89))
    assertEquals("""[{"foo":5},{"bar":89}]""", arr(dboList: _*).toJson)
  }

  @Test
  def option {
      def toObj(tuple: Option[(Int, Int)]): Option[DBObject] = tuple.map { case (first, second) ⇒ obj("first" := first, "second" := second) }
    val dbo = obj(
      "foo" := "bar",
      "tuple" := toObj(Some(4 -> 9)))
    assertEquals("""{"foo":"bar","tuple":{"first":4,"second":9}}""", dbo.toJson)
    val tuple = toObj(None)
    val dbo2 = obj(
      "foo" := "bar",
      "tuple" := tuple)
    assertEquals("""{"foo":"bar","tuple":null}""", dbo2.toJson)
  }

  @Test
  def oid {
    val dbo = obj("_id" := new ObjectId("506c53b0a025ec577423ef92"))
    assertEquals("""{"_id":{"$oid":"506c53b0a025ec577423ef92"}}""", dbo.toJson)
    val other: DBObject = dbo("_id").as[ObjectId]
    assertEquals(dbo.toJson, other.toJson)
  }

  @Test
  def in {
    val names = Array("foo", "bar")
    val filter = obj(
      $and(
        "key.day" := $gte(20120101),
        "key.day" := $lte(20121231)))
    filter.add("key.name" := $in(names: _*))
    assertEquals("""{"$and":[{"key.day":{"$gte":20120101}},{"key.day":{"$lte":20121231}}],"key.name":{"$in":["foo","bar"]}}""", filter.toJson)
  }

  @Test
  def array {
    implicit val ColCdc = new Codec[java.awt.Color, BsonValue] {
      def encode(color: java.awt.Color): BsonValue = IntCdc.encode(color.getRGB)
      def decode(bson: BsonValue) = bson.raw match {
        case i: Int ⇒ java.awt.Color.decode(i.toString)
      }
    }
    val array = Array(java.awt.Color.BLACK, java.awt.Color.BLUE)
    val doc = obj("array" := array)
    val newArray = Array[AnyRef](doc("array").asSeq[java.awt.Color]: _*)
    assertTrue(java.util.Arrays.equals(array.asInstanceOf[Array[Object]], newArray))
  }

  @Test
  def each {
    val newNames = List("Foo", "Bar")
    val update = $addToSet("names" := $each(newNames: _*))
    assertEquals("""{"$addToSet":{"names":{"$each":["Foo","Bar"]}}}""", update.toJson)
  }

  @Test
  def nullFilter {
    val dbo = obj("foo" := null).ignoreNulls(true)
    dbo.add("bar" := null)
    assertEquals(None, dbo("foo").opt[String])
    assertEquals(None, dbo("bar").opt[String])
    dbo("foo") match {
      case _: Null ⇒ assertTrue(true)
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
    assertEquals("{}", doc.toJson)
    val doc2 = obj(ignoreEmpty = true)
    doc2.add("foo" := null, "bar" := List.empty[Int])
    assertEquals("""{"foo":null}""", doc2.toJson)
  }

  @Test
  def listAsDBO {
    val dbo: DBObject = arr("A", "B", "C")
    assertEquals("""["A","B","C"]""", dbo.toJson)
    val list = dbo.asSeq[String]
    assertEquals(Seq("A", "B", "C"), list)
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

  @Test
  def rename {
    val doc = obj("foo" := 42)
    doc.rename("foo" -> "bar")
    assertEquals(None, doc("foo").opt[Int])
    assertEquals(42, doc("bar").as[Int])
  }

  @Test
  def scalaMap {
    val doc = obj("mymap" := Map("two" -> 2, "three" -> 3, "fortytwo" -> 42))
    assertEquals("""{"mymap":{"two":2,"three":3,"fortytwo":42}}""", doc.toJson)
    assertEquals(Map("two" -> 2, "three" -> 3, "fortytwo" -> 42), doc("mymap").as[Map[String, Int]])
  }

  @Test
  def mapReduceCoffee {
    val coll = new RichDBCollection(null)
    val map = "-> emit(@days[0].toString().substring(0,4), {count: 1}); return"
    val reduce = "(key, values) -> {count: values.reduce (t, v) -> t + v.count}"
    val mapReduce = MapReduce.coffee(map, reduce)
    println("Map:\n" + mapReduce.mapJS)
    println("Reduce:\n" + mapReduce.reduceJS)
    val coffeescript = new java.io.StringReader("""
map  =  -> emit(@days[0].toString().substring(0,4), {count: 1}); return
reduce=(key, values) -> {count: values.reduce (t, v) -> t + v.count}
      """)
    val fromFile = MapReduce.brew(coffeescript)
    assertEquals(mapReduce.mapJS, fromFile.mapJS)
    assertEquals(mapReduce.reduceJS, fromFile.reduceJS)
  }

  @Test
  def remove {
    val oid = new ObjectId
    val doc = obj("_id" := oid, "foo" := "bar", "nested" := obj("two" := 2, "three" := 3, "fortytwo" := 42))
    assertEquals(oid, doc.remove("_id").as[ObjectId])
    assertTrue(doc.remove("bar").opt[String].isEmpty)

    assertTrue(doc.contains("foo"))
    assertEquals("bar", doc.remove("foo").as[String])
    assertFalse(doc.contains("foo"))

    assertFalse(doc.contains("nested.fortyone"))
    assertTrue(doc.contains("nested.fortytwo"))
    assertEquals(42, doc.remove("nested.fortytwo").as[Int])
    assertFalse(doc.contains("nested.fortytwo"))
    assertTrue(doc.contains("nested"))
  }
  @Test
  def toJson {
    val floats = 34.5f :: Float.NaN :: Float.NegativeInfinity :: Float.PositiveInfinity :: Nil
    val floatDoc = obj("numbers" := floats)
    assertEquals("""{"numbers":[34.5,null,null,null]}""", floatDoc.toJson)
    val doubles = Double.NaN :: Double.NegativeInfinity :: Double.PositiveInfinity :: 123.45 :: Nil
    val dDoc = obj("numbers" := doubles)
    assertEquals("""{"numbers":[null,null,null,123.45]}""", dDoc.toJson)
  }

  @Test
  def `value props` {
    val foo = obj("foo" := $size(42))
    assertEquals("""{"foo":{"$size":42}}""", foo.toJson)
  }

  @Test
  def interfaces {
    trait Foo {
      def foo: String
      def nested: Bar
      trait Bar {
        def two: Int
        def three: Long
        def fortytwo: Double
        def baz: Option[String]
        def str(name: String): String
        def dbl(name: String): Double
        def any(name: String): Any
        def maybe(name: String): Option[Any]
      }
      def maybe: Option[Int]
      def list: Seq[String]
      def definite: Option[Int]
      def list2: Seq[Double]
    }
      def assertStuff(foo: Foo) {
        assertEquals("bar", foo.foo)
        assertEquals(2, foo.nested.two)
        assertEquals(2d, foo.nested.dbl("two"), 0.01)
        assertEquals("2", foo.nested.str("two"))
        assertEquals(2, foo.nested.any("two"))
        assertEquals(Some(2), foo.nested.maybe("two"))
        assertEquals(None, foo.nested.maybe("blablabla"))
        assertEquals(3L, foo.nested.three)
        assertEquals(42d, foo.nested.fortytwo, 0d)
        assertEquals(None, foo.maybe)
        assertEquals(Seq(), foo.list)
        assertEquals(666, foo.definite.get)
        assertEquals(Seq(1d, 2d, 3d), foo.list2)
        assertEquals("""{"foo":"bar","nested":{"two":2,"three":3,"fortytwo":42,"baz":null},"definite":666,"list2":[1.0,2.0,3.0]}""", foo.toString)
      }
    val doc = obj("foo" := "bar", "nested" := obj("two" := 2, "three" := 3, "fortytwo" := 42, "baz" := null), "definite" := 666, "list2" := arr(1, 2f, 3L))
    val foo = doc.like[Foo]
    assertStuff(foo)
    assertEquals(None, foo.nested.baz)
    parseJsonObject("""{"foo":"bar","nested":{"two":2,"three":3,"fortytwo":42,"baz":null},"definite":666,"list2":[1.0,2.0,3.0]}""").map(_.like[Foo]) match {
      case None ⇒ fail("Where's the object?")
      case Some(foo) ⇒ assertStuff(foo)
    }
  }

  @Test
  def `interface mismatch` {
    trait Foo {
      def baz: Option[Integer]
      def barbarbar: String
    }
    val missing = obj().like[Foo]
    assertEquals(None, missing.baz)
    try {
      missing.barbarbar
      fail("Should fail on unknown property")
    } catch {
      case e: UnavailableValueException ⇒ assertEquals("barbarbar", e.fieldName)
    }
    val asNull = obj("baz" := null, "barbarbar" := null).like[Foo]
    assertEquals(None, asNull.baz)
    try {
      asNull.barbarbar
      fail("Should fail on null property")
    } catch {
      case e: UnavailableValueException ⇒ assertEquals("barbarbar", e.fieldName)
    }
    val invalidInt = obj("baz" := "dsfgasdfasd").like[Foo]
    try {
      val notHappening = invalidInt.baz
      fail("Should fail on invalid Int: " + notHappening)
    } catch {
      case e: InvalidValueTypeException ⇒ assertEquals("baz", e.fieldName)
    }
  }

  @Test
  def `null or None` {
    val foo = obj("foo" := $ne(None))
    assertEquals("""{"foo":{"$ne":null}}""", foo.toJson)
    val bar = obj("bar" := obj("$ne" := null))
    assertEquals("""{"bar":{"$ne":null}}""", bar.toJson)
  }

  @Test
  def transform {
    val oid = new ObjectId
    val doc = obj("_id" := oid)
    doc.rename("_id" -> "identifier", id ⇒ id.as[String])
    assertEquals("""{"identifier":"%s"}""".format(oid), doc.toJson)
  }

  @Test
  def transformNull {
    val map = Map[ObjectId, Int]()
    val doc = obj()
    doc.rename("fooId" -> "foo", fooId ⇒
      fooId.opt[ObjectId].flatMap(map.get(_).map(_ * 2)))
    assertEquals("{}", doc.toJson)
  }

  @Test
  def transformMissing {
    val map = Map[ObjectId, Int]()
    val doc = obj()
    doc.rename("fooId" -> "foo", fooId ⇒
      fooId.opt[ObjectId].flatMap(map.get(_).map(_ * 2)))
    assertEquals("{}", doc.toJson)
  }

  @Test
  def `interface with Set` {
    val codec = new Codec[GeoPoint, BsonValue] {
      def encode(gp: GeoPoint): BsonValue = StrCdc.encode("%d %d".format(gp.latitude, gp.longitude))
      def decode(str: BsonValue): GeoPoint = GeoPoint.parse(StrCdc.decode(str)).get
    }
    trait Foo {
      def pts: Set[GeoPoint]
    }
    val doc = obj("pts" := arr("23.532 54.2342"))
    assertEquals("""{"pts":["23.532 54.2342"]}""", doc.toJson)
    implicit val mapping: Map[Class[_], Codec[_, BsonValue]] = Map(classOf[GeoPoint] -> codec)
    val foo = doc.like[Foo]
    val set = Set(new GeoPoint(23.532f, 54.2342f))
    assertEquals(set, foo.pts)
  }
  @Test
  def `interface with Map` {
    trait Foo {
      def map: Map[String, Integer]
    }
    val doc = obj(
      "map" := obj("one" := 1, "two" := 2, "three" := 3))
    val foo = doc.like[Foo]
    assertEquals(1, foo.map("one"))
    assertEquals(2, foo.map("two"))
    assertEquals(3, foo.map("three"))
  }

  @Test
  def `nested interface proxying` {
    trait Foo {
      trait Bar {
        def baz: Int
      }
      def bar: Option[Bar]
    }
    val foo = obj("bar" := obj("baz" := 666)).like[Foo]
    assertEquals(666, foo.bar.get.baz)
  }

  @Test
  def `nested list in map` {
    val map: Map[String, collection.immutable.IndexedSeq[String]] = Map("foo" -> Vector("1", "2"), "bar" -> Vector("3", "4"))
    val doc = obj("map" := map)
    assertEquals("""{"map":{"foo":["1","2"],"bar":["3","4"]}}""", doc.toJson)
  }

  @Test
  def emptyString() {
    val strCdc = new Codec[String, BsonValue] {
      def encode(a: String): BsonValue = new BsonValue { def raw = a }
      def decode(b: BsonValue): String = {
        val str = String valueOf b.raw
        if (str.length == 0) null else str
      }
    }
    val emptyStringAsNull: Map[Class[_], Codec[_, BsonValue]] = Map(classOf[String] -> strCdc)
    trait Foo {
      def bar: Option[String]
      def baz: String
    }
    val foo = obj("bar" := "", "baz" := "").like[Foo](ClassTag(classOf[Foo]), emptyStringAsNull)
    assertEquals(None, foo.bar)
    Try(foo.baz) match {
      case Failure(e: UnavailableValueException) ⇒ assertEquals("baz", e.fieldName)
      case Success(baz) ⇒ fail(s"`baz` should not succeed: $baz")
    }
  }

  @Test
  def optionalArray() {
    trait Foo {
      def arr: Option[Array[Int]]
    }
    trait Bar {
      def arr: IndexedSeq[Integer]
    }
    val doc = obj("arr" := arr(1, 2, 3))
    val foo = doc.like[Foo]
    foo.arr match {
      case None ⇒ fail("Should have array")
      case Some(array) ⇒ assertTrue(Arrays.equals(Array(1, 2, 3), array))
    }
    val bar = doc.like[Bar]
    assertEquals(Seq(1, 2, 3), bar.arr)
  }

  @Test
  def mapLocaleKey() {
    trait Foo {
      def byLang: Map[String, Array[String]]
    }
    val foo = obj("byLang" := obj("en" := arr("Hello", "World"), "es" := arr("Hola", "Mundo")))
    val byLang = foo.like[Foo].byLang.map {
      case (key, value) ⇒ Locale.forLanguageTag(key) -> value
    }
    assertEquals(Seq("Hello", "World"), byLang(Locale.ENGLISH).toSeq)
    assertEquals(Seq("Hola", "Mundo"), byLang(Locale.forLanguageTag("es")).toSeq)
  }

}
