package scuff

import java.util.{ Date, UUID, Locale, TimeZone }
import com.mongodb._
import org.bson.types._
import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag

/**
 * Convenience DSL for the MongoDB Java driver.
 */
object Mongolia {

  class UnavailableValueException(val fieldName: String, message: String) extends RuntimeException(message)
  class InvalidValueTypeException(val fieldName: String, message: String) extends RuntimeException(message)

  private lazy val coffeeCompiler = js.CoffeeScriptCompiler('bare -> true)

  final class Assignment(key: String) {
    def :=(value: BsonValue) = new BsonProp(key, value)
    def :=[T](value: T)(implicit codec: Codec[T, BsonValue]) = new BsonProp(key, codec.encode(value))
    def :=(value: Int) = new BsonIntProp(key, value)
    def :=(value: Long) = new BsonLngProp(key, value)
    def :=(value: Double) = new BsonDblProp(key, value)
  }
  implicit def toAssignment(key: String) = new Assignment(key)
  implicit def toAssignment(key: scala.Symbol) = new Assignment(key.name)

  class BsonProp(val key: String, value: BsonValue) {
    def raw = if (value == null) null else value.raw
    override def toString = key concat " : " concat String.valueOf(value.raw)
  }
  sealed trait BsonNumProp extends BsonProp
  final class BsonIntProp(key: String, intValue: Int) extends BsonProp(key, IntCdc.encode(intValue)) with BsonNumProp
  final class BsonLngProp(key: String, lngValue: Long) extends BsonProp(key, LongCdc.encode(lngValue)) with BsonNumProp
  final class BsonDblProp(key: String, dblValue: Double) extends BsonProp(key, DblCdc.encode(dblValue)) with BsonNumProp

  trait BsonValue { def raw: Any }
  implicit val NoneCdc = new Codec[None.type, BsonValue] {
    val NullBsonValue = new BsonValue { def raw = null }
    def encode(a: None.type): BsonValue = NullBsonValue
    def decode(b: BsonValue) = null
  }
  implicit val RgxCdc = new Codec[java.util.regex.Pattern, BsonValue] {
    def encode(rgx: java.util.regex.Pattern): BsonValue = new Value(rgx): BsonValue
    def decode(b: BsonValue) = java.util.regex.Pattern.compile(String.valueOf(b.raw))
  }
  //  implicit val Rgx2Val = (rgx: java.util.regex.Pattern) ⇒ new Value(rgx): BsonValue
  implicit val SRgxCdc = new Codec[scala.util.matching.Regex, BsonValue] {
    def encode(a: scala.util.matching.Regex): BsonValue = RgxCdc.encode(a.pattern)
    def decode(b: BsonValue) = new scala.util.matching.Regex(String.valueOf(b.raw))
  }
  implicit val StrCdc = new Codec[String, BsonValue] {
    def encode(a: String): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case _: DBObject ⇒ throw new RuntimeException("Cannot coerce DBObject into String")
      case _ ⇒ String.valueOf(b.raw)
    }
  }
  implicit val PropCdc = new Codec[BsonProp, BsonValue] {
    def encode(a: BsonProp): BsonValue = prop2obj(a)
    def decode(b: BsonValue) = b.raw match {
      case dbo: DBObject ⇒
        val keys = dbo.keySet()
        if (keys.size == 1) {
          val name = keys.iterator.next
          name := new Value(dbo.get(name))
        } else {
          throw new RuntimeException("Cannot extract single BsonProp when %d are available: %s".format(keys.size, keys))
        }
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into BsonProp".format(b.raw.getClass.getName))
    }
  }
  implicit val ClzCdc = new Codec[Class[_], BsonValue] {
    def encode(a: Class[_]) = StrCdc.encode(a.getName)
    def decode(b: BsonValue): Class[_] = Class.forName(StrCdc.decode(b))
  }

  implicit val IntPropCdc = new Codec[BsonIntProp, BsonValue] {
    def encode(a: BsonIntProp) = PropCdc.encode(a)
    def decode(b: BsonValue) = {
      val prop = PropCdc.decode(b)
      new BsonIntProp(prop.key, prop.raw.asInstanceOf[Int])
    }
  }
  implicit val DblPropCdc = new Codec[BsonDblProp, BsonValue] {
    def encode(a: BsonDblProp) = PropCdc.encode(a)
    def decode(b: BsonValue) = {
      val prop = PropCdc.decode(b)
      new BsonDblProp(prop.key, prop.raw.asInstanceOf[Double])
    }
  }
  implicit val LngPropCdc = new Codec[BsonLngProp, BsonValue] {
    def encode(a: BsonLngProp) = PropCdc.encode(a)
    def decode(b: BsonValue) = {
      val prop = PropCdc.decode(b)
      new BsonLngProp(prop.key, prop.raw.asInstanceOf[Long])
    }
  }
  implicit val IntCdc = new Codec[Int, BsonValue] {
    def encode(a: Int): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case str: String ⇒ str.toInt
      case _ ⇒ org.bson.BSON.toInt(b.raw)
    }
  }
  implicit val LongCdc = new Codec[Long, BsonValue] {
    def encode(a: Long): BsonValue = new Value(a)
    def decode(b: BsonValue): Long = b.raw match {
      case n: Number ⇒ n.longValue()
      case d: java.util.Date ⇒ d.getTime
      case s: String ⇒ s.toLong
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Long".format(b.raw.getClass.getName))
    }
  }
  implicit val DblCdc = new Codec[Double, BsonValue] {
    def encode(a: Double): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case n: Number ⇒ n.doubleValue()
      case s: String ⇒ s.toDouble
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Double".format(b.raw.getClass.getName))
    }
  }
  implicit val FltCdc = new Codec[Float, BsonValue] {
    def encode(a: Float): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case n: Number ⇒ n.floatValue()
      case s: String ⇒ s.toFloat
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Float".format(b.raw.getClass.getName))
    }
  }
  implicit val ShrtCdc = new Codec[Short, BsonValue] {
    def encode(a: Short): BsonValue = new Value(a)
    def decode(b: BsonValue): Short = b.raw match {
      case n: Number ⇒ n.shortValue()
      case s: String ⇒ s.toShort
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Short".format(b.raw.getClass.getName))
    }
  }
  implicit val ByteCdc = new Codec[Byte, BsonValue] {
    def encode(a: Byte): BsonValue = new Value(a)
    def decode(b: BsonValue): Byte = b.raw match {
      case n: Number ⇒ n.byteValue
      case s: String ⇒ s.toByte
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Short".format(b.raw.getClass.getName))
    }
  }
  implicit val BoolCdc = new Codec[Boolean, BsonValue] {
    def encode(a: Boolean): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case b: java.lang.Boolean ⇒ b.booleanValue
      case _ ⇒ IntCdc.decode(b) != 0
    }
  }
  implicit val BACdc = new Codec[Array[Byte], BsonValue] {
    def encode(a: Array[Byte]): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case arr: Array[Byte] ⇒ arr
      case bin: Binary ⇒ bin.getData
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Array[Byte]".format(b.raw.getClass.getName))
    }
  }
  implicit val BinCdc = new Codec[Binary, BsonValue] {
    def encode(a: Binary): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case arr: Array[Byte] ⇒ new Binary(arr)
      case bin: Binary ⇒ bin
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Binary".format(b.raw.getClass.getName))
    }
  }
  implicit val DateCdc = new Codec[Date, BsonValue] {
    def encode(a: Date): BsonValue = new Value(a)
    def decode(b: BsonValue): Date = b.raw match {
      case ts: Timestamp ⇒ new Date(ts.asMillis)
      case d: Date ⇒ d
      case l: Long ⇒ new Date(l.longValue)
      case oid: ObjectId ⇒ new Date(oid.getTime)
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Date".format(b.raw.getClass.getName))
    }
  }
  implicit val TsCdc = new Codec[Timestamp, BsonValue] {
    def encode(a: Timestamp): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case ts: Timestamp ⇒ ts
      case d: Date ⇒ new Timestamp(d)
      case l: Long ⇒ new Timestamp(l.longValue)
      case oid: ObjectId ⇒ new Timestamp(oid.getTime)
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Timestamp".format(b.raw.getClass.getName))
    }
  }
  implicit val OIDCdc = new Codec[ObjectId, BsonValue] {
    def encode(a: ObjectId): BsonValue = new Value(a)
    def decode(b: BsonValue) = ObjectId.massageToObjectId(b.raw)
  }
  implicit val UUIDCdc = new Codec[UUID, BsonValue] {
    def encode(uuid: UUID): BsonValue = {
      val bb = java.nio.ByteBuffer.allocate(16)
      bb.putLong(uuid.getMostSignificantBits).putLong(uuid.getLeastSignificantBits)
      new Value(new Binary(4, bb.array))
    }
    def decode(b: BsonValue) = b.raw match {
      case u: UUID ⇒ u
      case b: Binary if b.getType == 4 ⇒ binaryType4ToUUID(b.getData)
      case a: Array[Byte] if a.length == 16 ⇒ binaryType4ToUUID(a)
      case s: String if s.length == 36 ⇒ UUID.fromString(s)
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into UUID".format(b.raw.getClass.getName))
    }
  }
  implicit val PwdCdc = new Codec[Password, BsonValue] {
    def encode(a: Password): BsonValue = {
      val dbo = obj("digest" := a.digest, "algo" := a.algorithm)
      if (a.salt.length > 0) dbo.add("salt" := a.salt)
      if (a.iterations > 1) dbo.add("iter" := a.iterations)
      new Value(dbo)
    }
    def decode(b: BsonValue) = b.raw match {
      case obj: DBObject ⇒
        val dbo = enrich(obj)
        new Password(dbo("digest").as[Array[Byte]], dbo("algo").as[String], dbo("salt").opt[Array[Byte]].getOrElse(Array.empty), dbo("iter").opt[Int].getOrElse(1))
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into Password".format(b.raw.getClass.getName))
    }
  }
  implicit val EmlCdc = new Codec[EmailAddress, BsonValue] {
    def encode(a: EmailAddress): BsonValue = new Value(a.toString)
    def decode(b: BsonValue) = new EmailAddress(String.valueOf(b.raw))
  }
  implicit val UrlCdc = new Codec[java.net.URL, BsonValue] {
    def encode(a: java.net.URL): BsonValue = new Value(a.toString)
    def decode(b: BsonValue) = new java.net.URL(String.valueOf(b.raw))
  }
  implicit val UriCdc = new Codec[java.net.URI, BsonValue] {
    def encode(a: java.net.URI): BsonValue = new Value(a.toString)
    def decode(b: BsonValue) = new java.net.URI(String.valueOf(b.raw))
  }
  implicit val TzCdc = new Codec[TimeZone, BsonValue] {
    def encode(a: TimeZone): BsonValue = new Value(a.getID)
    def decode(b: BsonValue) = TimeZone.getTimeZone(String.valueOf(b.raw))
  }
  implicit val BDCdc = new Codec[BigDecimal, BsonValue] {
    def encode(a: BigDecimal): BsonValue = new Value(a.toString)
    def decode(b: BsonValue): BigDecimal = b.raw match {
      case bd: java.math.BigDecimal ⇒ BigDecimal(bd)
      case s: String ⇒ BigDecimal(s)
      case d: Double ⇒ BigDecimal(d)
      case i: Int ⇒ BigDecimal(i)
      case l: Long ⇒ BigDecimal(l)
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into BigDecimal".format(b.raw.getClass.getName))
    }
  }
  private def geo2Dbo(gp: GeoPoint): RichDBObject = obj("type" := "Point", "coordinates" := arr(gp.longitude: Double, gp.latitude: Double))
  implicit def GeoPointCdc = GeoCdc
  private[this] val GeoCdc = new Codec[GeoPoint, BsonValue] {
    def encode(a: GeoPoint): BsonValue = {
      val dbo = geo2Dbo(a)
      if (a.radius > 0f) dbo.add("radius" := a.radius)
      dbo: BsonValue
    }
    def decode(b: BsonValue): GeoPoint = b.raw match {
      case dbo: DBObject ⇒
        if (dbo.isInstanceOf[java.util.List[_]]) {
          val coords = b.raw.asInstanceOf[java.util.List[Number]]
          new GeoPoint(coords.get(1).floatValue, coords.get(0).floatValue, 0f)
        } else {
          val coords = dbo.getAs[java.util.List[Number]]("coordinates")
          val radius: Float = dbo("radius").opt[Float].getOrElse(0f)
          new GeoPoint(coords.get(1).floatValue, coords.get(0).floatValue, radius)
        }
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into GeoPoint".format(b.raw.getClass.getName))
    }
  }

  implicit val LocCdc = new Codec[Locale, BsonValue] {
    def encode(a: Locale): BsonValue = new Value(a.toLanguageTag)
    def decode(b: BsonValue) = Locale.forLanguageTag(String.valueOf(b.raw))
  }
  implicit def OptCdc[T](implicit codec: Codec[T, BsonValue]) = new Codec[Option[T], BsonValue] {
    def encode(a: Option[T]) = a match {
      case Some(t) ⇒ codec.encode(t)
      case _ ⇒ null
    }
    def decode(b: BsonValue) = b.raw match {
      case null ⇒ None
      case _ ⇒ Some(codec.decode(b))
    }
  }
  implicit def DBObjectCdc: Codec[DBObject, BsonValue] = DboCdc
  private[this] val DboCdc = new Codec[DBObject, BsonValue] {
    def encode(a: DBObject): BsonValue = a match {
      case l: RichDBList ⇒ l
      case _ ⇒ enrich(a)
    }
    def decode(b: BsonValue): DBObject = b.raw match {
      case list: BasicDBList ⇒ list
      case list: org.bson.LazyDBList ⇒ list
      case dbo: DBObject ⇒ dbo.enrich: DBObject
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into DBObject".format(b.raw.getClass.getName))
    }
  }
  implicit def RichDBObjectCdc: Codec[RichDBObject, BsonValue] = RDboCdc
  private[this] val RDboCdc = new Codec[RichDBObject, BsonValue] {
    def encode(a: RichDBObject): BsonValue = a
    def decode(b: BsonValue): RichDBObject = b.raw match {
      case dbo: RichDBObject ⇒ dbo
      case dbo: DBObject ⇒ dbo.enrich
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into RichDBObject".format(b.raw.getClass.getName))
    }
  }
  implicit def MapCdc[T](implicit codec: Codec[T, BsonValue]) = new Codec[Map[String, T], BsonValue] {
    def encode(a: Map[String, T]): BsonValue = {
      val dbo = new RichDBObject
      dbo.add(a)
      dbo
    }
    private def toValue(value: Any): T = if (value == null) null.asInstanceOf[T] else codec.decode(new Value(value))
    def decode(b: BsonValue): Map[String, T] = {
      var map: Map[String, T] = Map.empty
      b.raw match {
        case jmap: java.util.Map[_, _] ⇒
          val iter = jmap.asInstanceOf[java.util.Map[String, Any]].entrySet.iterator
          while (iter.hasNext) {
            val entry = iter.next
            map += (entry.getKey -> toValue(entry.getValue))
          }
        case dbo: DBObject ⇒
          val keys = dbo.keys.iterator
          while (keys.hasNext) {
            val key = keys.next
            map += key -> toValue(dbo.get(key))
          }
      }
      map
    }
  }
  private def any2Array[T](any: Any)(implicit codec: Codec[T, BsonValue], tag: ClassTag[T]): Array[T] = {
    val list: Iterable[_] = anyToIterable(any)
    val array = new Array[T](list.size)
    var i = 0
    val iter = list.iterator
    while (i < array.length) {
      iter.next match {
        case null ⇒ // Ignore
        case a ⇒ array(i) = codec.decode(new Value(a))
      }
      i += 1
    }
    array
  }

  private def seq2bsonlist[T](seq: GenTraversableOnce[T])(implicit codec: Codec[T, BsonValue]) = {
    val list = new RichDBList
    seq.foreach(t ⇒ list += codec.encode(t).raw)
    list
  }

  implicit def ArrayListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[Array[T], BsonValue] {
    def encode(a: Array[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): Array[T] = any2Array(b.raw)
  }
  implicit def IdxSeqListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[IndexedSeq[T], BsonValue] {
    def encode(a: IndexedSeq[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): IndexedSeq[T] = any2Array(b.raw)(codec, tag)
  }
  implicit def iIdxSeqListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[collection.immutable.IndexedSeq[T], BsonValue] {
    def encode(a: collection.immutable.IndexedSeq[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): collection.immutable.IndexedSeq[T] = Vector(any2Array(b.raw)(codec, tag): _*)
  }
  implicit def SeqListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[Seq[T], BsonValue] {
    def encode(a: Seq[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): Seq[T] = any2Array(b.raw)(codec, tag)
  }
  implicit def ListListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[List[T], BsonValue] {
    def encode(a: List[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): List[T] = any2Array(b.raw)(codec, tag).toList
  }
  implicit def IterListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[Iterable[T], BsonValue] {
    def encode(a: Iterable[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): Iterable[T] = any2Array(b.raw)(codec, tag)
  }
  implicit def TravListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[Traversable[T], BsonValue] {
    def encode(a: Traversable[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): Traversable[T] = any2Array(b.raw)(codec, tag)
  }
  implicit def SetListCdc[T](implicit codec: Codec[T, BsonValue], tag: ClassTag[T]) = new Codec[Set[T], BsonValue] {
    def encode(a: Set[T]): BsonValue = seq2bsonlist(a)
    def decode(b: BsonValue): Set[T] = any2Array(b.raw)(codec, tag).toSet
  }

  implicit def id2obj(oid: ObjectId): DBObject = obj("_id" := oid)

  def obj(ignoreNulls: Boolean = false, ignoreEmpty: Boolean = false): RichDBObject = new RichDBObject(ignoreNulls = ignoreNulls, ignoreEmpty = ignoreEmpty)
  def obj(props: Seq[BsonProp]): RichDBObject = {
    val map = new RichDBObject
    props.foreach(p ⇒ if (map.put(p.key, p.raw) != null) throw new IllegalArgumentException("Field \"%s\" occurs multiple times".format(p.key)))
    map
  }
  def obj(head: BsonProp, tail: BsonProp*): RichDBObject = {
    val map = new RichDBObject(new BasicDBObject(head.key, head.raw))
    tail.foreach(p ⇒ if (map.put(p.key, p.raw) != null) throw new IllegalArgumentException("Field \"%s\" occurs multiple times".format(p.key)))
    map
  }
  def arr(values: BsonValue*): RichDBList = seq2bsonlist(values)(Codec.noop)
  def arr[T](values: T*)(implicit codec: Codec[T, BsonValue]): RichDBList = seq2bsonlist(values)
  def obj[T](map: collection.Map[String, T])(implicit codec: Codec[T, BsonValue]): RichDBObject = new RichDBObject().add(map)

  implicit def enrich(poor: DBObject) = poor match {
    case rich: RichDBObject ⇒ rich
    case _ ⇒ new RichDBObject(poor)
  }
  implicit def enrich(cursor: DBCursor) = new RichDBCursor(cursor)
  implicit def enrich(coll: DBCollection) = new RichDBCollection(coll)
  implicit def enrich(db: DB) = new RichDB(db)
  implicit def impoverish(rich: RichDBObject) = rich.impoverish
  implicit def impoverish(rich: RichDBCollection) = rich.impoverish
  implicit def impoverish(rich: RichDBCursor) = rich.impoverish
  implicit def impoverish(rich: RichDB) = rich.impoverish

  sealed abstract class BsonType(val typeNumber: Byte)
  object BsonType {
    case object Double extends BsonType(1)
    case object String extends BsonType(2)
    case object Object extends BsonType(3)
    case object Array extends BsonType(4)
    case object Binary extends BsonType(5)
    case object OID extends BsonType(7)
    case object Boolean extends BsonType(8)
    case object Date extends BsonType(9)
    case object Null extends BsonType(10)
    case object Regexp extends BsonType(11)
    case object Javascript extends BsonType(13)
    case object Symbol extends BsonType(14)
    case object JavascriptWithScope extends BsonType(15)
    case object Int extends BsonType(16)
    case object MongoTimestamp extends BsonType(17)
    case object Long extends BsonType(18)
    case object MinKey extends BsonType(-1)
    case object MaxKey extends BsonType(127)
  }

  val ASC, LAST, INCLUDE = 1
  val DESC, FIRST = -1
  val EXCLUDE = 0

  private def binaryType4ToUUID(array: Array[Byte]): UUID = {
    val bb = java.nio.ByteBuffer.wrap(array)
    new UUID(bb.getLong, bb.getLong)
  }

  private def anyToIterable(any: Any): Iterable[_] = {
    import collection.JavaConversions._
    any match {
      case arr: Array[AnyRef] ⇒ arr
      case list: java.lang.Iterable[_] ⇒ list
      case seq: collection.GenTraversableOnce[_] ⇒ seq.toIterable.asInstanceOf[Iterable[_]]
      case _ ⇒ throw new RuntimeException("Cannot coerce %s into java.util.List".format(any.getClass.getName))
    }
  }

  object BsonField {
    def apply(obj: Any, from: DBObject = null, key: String = null): BsonField = obj match {
      case null ⇒ if (from == null || from.containsField(key)) new Null(Option(key)) else new Missing(Option(key))
      case obj ⇒ new Value(obj)
    }
    def apply(obj: DBObject, key: String): BsonField = apply(obj.get(key), obj, key)
  }
  sealed trait BsonField {
    def opt[T](implicit codec: Codec[T, BsonValue]): Option[T]
    def as[T](implicit codec: Codec[T, BsonValue]): T
    def asSeq[T](implicit codec: Codec[T, BsonValue]): IndexedSeq[T]
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]): IndexedSeq[Option[T]]
    def asList[T](implicit codec: Codec[T, BsonValue]): List[T]
  }
  final class Null private[Mongolia] (fieldName: Option[String]) extends BsonField {
    def opt[T](implicit codec: Codec[T, BsonValue]) = None
    def as[T](implicit codec: Codec[T, BsonValue]): T = fieldName match {
      case None ⇒ throw new UnavailableValueException("", "Field value is null")
      case Some(name) ⇒ throw new UnavailableValueException(name, "Field \"%s\" value is null".format(name))
    }
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asSeq[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asList[T](implicit codec: Codec[T, BsonValue]) = Nil
  }
  final class Missing private[Mongolia] (fieldName: Option[String]) extends BsonField {
    def opt[T](implicit codec: Codec[T, BsonValue]) = None
    def as[T](implicit codec: Codec[T, BsonValue]): T = fieldName match {
      case None ⇒ throw new UnavailableValueException("", "Unknown field")
      case Some(name) ⇒ throw new UnavailableValueException(name, "Unknown field: \"%s\"".format(name))
    }
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asSeq[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asList[T](implicit codec: Codec[T, BsonValue]) = Nil
  }
  final class Value private[Mongolia] (val raw: Any) extends BsonField with BsonValue {
    override def toString() = "%s = %s".format(raw.getClass.getName, raw)
    def opt[T](implicit codec: Codec[T, BsonValue]): Option[T] = Some(codec.decode(this))
    def as[T](implicit codec: Codec[T, BsonValue]): T = codec.decode(this)
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]): IndexedSeq[Option[T]] = {
      val list: Iterable[_] = anyToIterable(raw)
      val array = new Array[Option[T]](list.size)
      var i = 0
      val iter = list.iterator
      while (i < array.length) {
        array(i) = iter.next match {
          case null ⇒ None
          case a ⇒ Some(codec.decode(new Value(a)))
        }
        i += 1
      }
      array
    }
    def asSeq[T](implicit codec: Codec[T, BsonValue]): IndexedSeq[T] = {
      val list: Iterable[_] = anyToIterable(raw)
      val array = new Array[Any](list.size)
      var i = 0
      val iter = list.iterator
      while (i < array.length) {
        iter.next match {
          case null ⇒ // Ignore
          case a ⇒ array(i) = codec.decode(new Value(a))
        }
        i += 1
      }
      array.toIndexedSeq.asInstanceOf[IndexedSeq[T]]
    }
    def asList[T](implicit codec: Codec[T, BsonValue]) = {
      val iterable: Iterable[_] = anyToIterable(raw)
      var list: List[T] = Nil
      var i = 0
      val iter = iterable.iterator
      while (iter.hasNext) {
        iter.next match {
          case null ⇒ // Ignore
          case a ⇒ list ::= codec.decode(new Value(a))
        }
      }
      list.reverse
    }
  }

  final class RichDB(underlying: DB) {
    def impoverish = underlying
    def apply(collection: String, wc: WriteConcern = null) = {
      val dbColl = underlying.getCollection(collection): RichDBCollection
      if (wc != null) dbColl.setWriteConcern(wc)
      dbColl
    }
    def enrich: RichDB = this
  }

  final class RichDBCollection(underlying: DBCollection) {
    implicit def impoverish = underlying
    private def SAFE = if (underlying.getWriteConcern.getW >= WriteConcern.SAFE.getW) underlying.getWriteConcern else WriteConcern.SAFE
    def safeInsert(dbo: DBObject) = underlying.insert(dbo, SAFE)
    def safeInsert(dbos: DBObject*) = underlying.insert(dbos.toArray, SAFE)
    def safeSave(dbo: DBObject) = underlying.save(dbo, SAFE)
    def safeUpdate(key: DBObject, upd: DBObject, upsert: Boolean = false, multi: Boolean = false) = underlying.update(key, upd, upsert, multi, SAFE)
    def safeUpdateMulti(key: DBObject, upd: DBObject) = underlying.update(key, upd, false, true, SAFE)
    def safeUpdateAtomic(key: DBObject, upd: DBObject) = {
      key.put("$atomic", true)
      safeUpdateMulti(key, upd)
    }
    def updateAtomic(key: DBObject, upd: DBObject) = {
      key.put("$atomic", true)
      underlying.updateMulti(key, upd)
    }
    def upsert(key: DBObject, upd: DBObject, concern: WriteConcern = underlying.getWriteConcern) = underlying.update(key, upd, true, false, concern)
    def safeUpsert(key: DBObject, upd: DBObject) = underlying.update(key, upd, true, false, SAFE)
    def safeRemove(key: DBObject) = underlying.remove(key, SAFE)
    def safeRemoveAtomic(key: DBObject) = {
      key.put("$atomic", true)
      underlying.remove(key, SAFE)
    }

    def unique(field: String, query: DBObject = null): Seq[BsonField] = {
      import collection.JavaConverters._
      val list = underlying.distinct(field, query).asInstanceOf[java.util.List[Any]]
      list.asScala.view.map(BsonField(_, null, field))
    }

    def mapReduceInline(mapReduce: MapReduce, query: DBObject = null): Iterator[DBObject] = {
      val javaIter = underlying.mapReduce(mapReduce.mapJS, mapReduce.reduceJS, null, MapReduceCommand.OutputType.INLINE, query).results.iterator
      new Iterator[DBObject] {
        def hasNext = javaIter.hasNext
        def next = javaIter.next
      }
    }

    private def mapReduceInto(mapReduce: MapReduce, coll: DBCollection, query: DBObject, outType: MapReduceCommand.OutputType) = {
      underlying.mapReduce(mapReduce.mapJS, mapReduce.reduceJS, coll.getFullName, outType, query)
    }
    def mapReduceMerge(mapReduce: MapReduce, mergeThis: DBCollection, query: DBObject = null): MapReduceOutput = {
      mapReduceInto(mapReduce, mergeThis, query, MapReduceCommand.OutputType.MERGE)
    }
    def mapReduceReduce(mapReduce: MapReduce, reduceThis: DBCollection, query: DBObject = null): MapReduceOutput = {
      mapReduceInto(mapReduce, reduceThis, query, MapReduceCommand.OutputType.REDUCE)
    }
    def mapReduceReplace(mapReduce: MapReduce, replaceThis: DBCollection, query: DBObject = null): MapReduceOutput = {
      mapReduceInto(mapReduce, replaceThis, query, MapReduceCommand.OutputType.REPLACE)
    }

    def ensureIndex(key: String): Unit = ensureIndex(obj(key := ASC))
    def ensureIndex(key: String, idxType: String): Unit = underlying.ensureIndex(obj(key := idxType))
    def ensureIndex(keyHead: BsonIntProp, keyTail: BsonIntProp*): Unit = underlying.ensureIndex(obj(keyHead, keyTail: _*))
    def ensureUniqueIndex(key: String): Unit = underlying.ensureIndex(obj(key := ASC), obj("unique" := true))
    def ensureHashedIndex(key: String): Unit = underlying.ensureIndex(obj(key := "hashed"))
    def ensureSparseIndex(key: String): Unit = underlying.ensureIndex(obj(key := ASC), obj("sparse" := true))
    def ensureUniqueSparseIndex(key: String): Unit = underlying.ensureIndex(obj(key := ASC), obj("sparse" := true, "unique" := true))
    def ensureUniqueIndex(keyHead: BsonIntProp, keyTail: BsonIntProp*): Unit = underlying.ensureIndex(obj(keyHead, keyTail: _*), obj("unique" := true))
    def findOne(anyRef: Object) = anyRef match {
      case key: DBObject ⇒ underlying.findOne(key)
      case prop: BsonProp ⇒ underlying.findOne(obj(prop))
      case value: BsonValue ⇒ underlying.findOne(obj("_id" := value))
      case _ ⇒ underlying.findOne(anyRef)
    }
    def findOpt(key: DBObject, fields: DBObject = null) = underlying.findOne(key, fields) match {
      case null ⇒ None
      case doc ⇒ Some(doc.enrich)
    }
  }

  private val serializers = new ThreadLocal[com.mongodb.util.ObjectSerializer] {
    override def initialValue = com.mongodb.util.JSONSerializers.getStrict
  }

  private val base64 = new com.mongodb.util.Base64Codec

  /**
   * Much faster and more compact serialization,
   * and more importantly *correct* JSON, i.e.
   * `NaN` is translated to `null`.
   */
  def toJson(dbo: DBObject): String = {
    val fallback = serializers.get
    val sb = new java.lang.StringBuilder(128)
      def appendList(list: collection.GenTraversableOnce[_]) {
        sb append '['
        val i = list.toIterator
        var first = true
        while (i.hasNext) {
          if (first)
            first = false
          else
            sb append ','
          append(i.next)
        }
        sb append ']'
      }
      def appendMap(map: java.util.Map[_, _]) {
        import language.existentials
        sb append '{'
        val i = map.entrySet.iterator
        var first = true
        while (i.hasNext) {
          val entry = i.next
          if (first)
            first = false
          else
            sb append ','
          appendString(String.valueOf(entry.getKey))
          sb append ':'
          append(entry.getValue)
        }
        sb append '}'
      }
      def appendString(str: String) {
        sb append "\""
        var i = 0
        while (i < str.length) {
          str.charAt(i) match {
            case '\\' ⇒ sb append "\\\\"
            case '"' ⇒ sb append "\\\""
            case '\n' ⇒ sb append "\\n"
            case '\r' ⇒ sb append "\\r"
            case '\t' ⇒ sb append "\\t"
            case '\b' ⇒ sb append "\\b"
            case c if c < 32 ⇒ // Ignore
            case c ⇒ sb append c
          }
          i += 1
        }
        sb append "\""
      }
      def append(any: Any): Unit = any match {
        case null ⇒ sb append "null"
        case s: String ⇒ appendString(s)
        case d: Double ⇒ if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) sb append "null" else sb append d
        case i: Int ⇒ sb append i
        case l: Long ⇒ sb append l
        case b: Boolean ⇒ sb append b
        case d: java.util.Date ⇒ sb append "{\"$date\":" append d.getTime append '}'
        case id: ObjectId ⇒ sb append "{\"$oid\":\"" append id.toString append "\"}"
        case u: UUID ⇒ sb append "{\"$uuid\":\"" append u.toString append "\"}"
        case a: Array[AnyRef] ⇒ appendList(a)
        case b: Binary ⇒ if (b.getType == 4) {
          sb append "{\"$uuid\":\"" append binaryType4ToUUID(b.getData).toString append "\"}"
        } else {
          sb append "{\"$binary\":\"" append base64.encode(b.getData) append "\",\"$type\":" append b.getType append '}'
        }
        case r: RichDBObject ⇒ appendRef(r.impoverish)
        case f: Float ⇒ if (java.lang.Float.isNaN(f) || java.lang.Float.isInfinite(f)) sb append "null" else sb append f
        case _ ⇒ appendRef(any.asInstanceOf[AnyRef])
      }
      def appendRef(anyRef: AnyRef): Unit = {
        import collection.JavaConverters._
        anyRef match {
          case m: java.util.Map[_, _] ⇒ appendMap(m)
          case l: java.lang.Iterable[_] ⇒ appendList(l.asScala)
          case t: collection.GenTraversableOnce[_] ⇒ appendList(t)
          case _ ⇒ fallback.serialize(anyRef, sb)
        }
      }

    append(dbo)
    sb.toString
  }

  final class RichDBObject(private val underlying: DBObject = new BasicDBObject, ignoreNulls: Boolean = false, ignoreEmpty: Boolean = false) extends DBObject with BsonValue {
    import collection.JavaConverters._
    if (underlying == null) throw new NullPointerException("Document is null")
    def markAsPartialObject = underlying.markAsPartialObject
    def isPartialObject: Boolean = underlying.isPartialObject
    def put(key: String, v: Any) = v match {
      case null if ignoreNulls ⇒ underlying.removeField(key)
      case l: java.util.List[_] if ignoreEmpty && l.isEmpty ⇒ underlying.removeField(key)
      case _ ⇒ underlying.put(key, v)
    }
    def putAll(o: org.bson.BSONObject) = o match {
      case m: java.util.Map[_, _] ⇒ putAll(m: java.util.Map[_, _])
      case _ ⇒ for (k ← o.keySet.asScala) put(k, o.get(k))
    }
    def putAll(m: java.util.Map[_, _]) = for (entry ← m.entrySet.asScala) put(entry.getKey.toString, entry.getValue)
    def get(key: String) = underlying.get(key)
    def toMap = underlying.toMap
    def removeField(key: String) = underlying.removeField(key)
    @deprecated(message = "Deprecated", since = "Don't care")
    def containsKey(s: String) = underlying.containsKey(s)
    def containsField(s: String) = underlying.containsField(s)
    def keySet = underlying.keySet
    def merge(dbo: DBObject): RichDBObject = {
      dbo match {
        case r: RichDBObject ⇒ putAll(r.underlying)
        case _ ⇒ putAll(dbo)
      }
      this
    }
    def add[T](map: collection.Map[String, T])(implicit codec: Codec[T, BsonValue]): RichDBObject = {
      map.foreach {
        case (key, value) ⇒ add(key := value)
      }
      this
    }
    def rename(fromTo: (String, String)): Unit = rename(fromTo, null)(null)
    def rename[T](fromTo: (String, String), transform: BsonField ⇒ T)(implicit codec: Codec[T, BsonValue]): Unit = {
      if (underlying.containsField(fromTo._1)) {
        val removed = underlying.removeField(fromTo._1)
        if (transform == null) {
          underlying.put(fromTo._2, removed)
        } else {
          val value = transform(BsonField(removed, null, fromTo._1))
          this.add(fromTo._2 := value)
        }
      }
    }
    def raw = this
    /**
     * Ignore `null` values, i.e. don't put them into doc.
     */
    def ignoreNulls(ignore: Boolean): RichDBObject = if (ignore == this.ignoreNulls) this else new RichDBObject(underlying, ignore, this.ignoreEmpty)
    /**
     * Ignore empty arrays, i.e. don't put them into doc.
     */
    def ignoreEmpty(ignore: Boolean): RichDBObject = if (ignore == this.ignoreEmpty) this else new RichDBObject(underlying, this.ignoreNulls, ignore)
    def keys: Iterable[String] = underlying.keySet.asScala
    /**
     * Much faster and more compact serialization,
     * and more importantly *correct* JSON.
     */
    def toJson() = Mongolia.toJson(underlying)
    override def toString = underlying.toString()
    import java.util.StringTokenizer
    import collection.JavaConverters._

    @annotation.tailrec
    private[this] def getNested(obj: DBObject, nested: StringTokenizer, remove: Boolean): Any = {
      if (obj == null) {
        null
      } else {
        val name = nested.nextToken()
        if (nested.hasMoreTokens()) {
          getNested(obj.get(name).asInstanceOf[DBObject], nested, remove)
        } else if (remove) {
          obj.removeField(name)
        } else {
          obj.get(name)
        }
      }
    }
    @annotation.tailrec
    private[this] def containsNested(obj: DBObject, nested: StringTokenizer): Boolean = {
      if (obj == null) {
        false
      } else {
        val name = nested.nextToken()
        if (nested.hasMoreTokens()) {
          containsNested(obj.get(name).asInstanceOf[DBObject], nested)
        } else {
          obj.containsField(name)
        }
      }
    }

    def like[T](implicit tag: ClassTag[T], mapping: Map[Class[_], Codec[_, BsonValue]] = Map.empty): T =
      if (tag.runtimeClass.isInterface) Proxying.getProxy(this, mapping) else throw new IllegalArgumentException("%s must be an interface".format(tag.runtimeClass))

    def isEmpty = underlying match {
      case m: java.util.Map[_, _] ⇒ m.isEmpty
      case _ ⇒ underlying.keySet.isEmpty
    }
    def prop(key: String): BsonProp = new BsonProp(key, new Value(underlying.get(key)))
    def asSeq[T](implicit codec: Codec[T, BsonValue]) = new Value(underlying).asSeq[T]
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]) = new Value(underlying).asSeqOfOption[T]
    def add(head: BsonProp, tail: BsonProp*): RichDBObject = {
      this.put(head.key, head.raw)
      tail.foreach { prop ⇒
        this.put(prop.key, prop.raw)
      }
      this
    }
    def enrich = this
    def impoverish = underlying
    def _id = underlying.get("_id") match {
      case null ⇒ throw new IllegalArgumentException("Field \"_id\" is missing")
      case oid: ObjectId ⇒ oid
      case any ⇒ ObjectId.massageToObjectId(any)
    }
    def apply(key: String): BsonField = BsonField(getAs(key), underlying, key)
    def apply(head: BsonProp, tail: BsonProp*): RichDBObject = add(head, tail: _*)
    def has(key: String) = this.contains(key)
    def getAs[T](name: String): T = {
      if (name.indexOf('.') == -1) {
        underlying.get(name).asInstanceOf[T]
      } else {
        getNested(underlying, new java.util.StringTokenizer(name, "."), remove = false).asInstanceOf[T]
      }
    }

    def remove(key: String): BsonField = {
      val value = if (key.indexOf('.') == -1) {
        underlying.removeField(key)
      } else {
        getNested(underlying, new java.util.StringTokenizer(key, "."), remove = true)
      }
      BsonField(value, null, key)
    }

    def contains(name: String): Boolean = {
      if (name.indexOf('.') == -1) {
        underlying.containsField(name)
      } else {
        containsNested(underlying, new java.util.StringTokenizer(name, "."))
      }
    }

    def map[T](f: RichDBObject ⇒ T): T = f(this)

  }

  final class RichDBList extends BasicDBList with BsonValue {
    def raw = this
    /**
     * Much faster and more compact serialization,
     * and more importantly *correct* JSON, i.e.
     * `NaN` is translated to `null`.
     */
    def toJson() = Mongolia.toJson(this)
    def +=(any: Any) = add(any.asInstanceOf[Object])
  }

  final class RichDBCursor(cursor: DBCursor) {
    def impoverish = cursor
    override def toString = map(_.toString).mkString("[", ",", "]")
    def toSeq() = map(d ⇒ d)
    def find(foundIt: RichDBObject ⇒ Boolean): Option[RichDBObject] = {
      var found: Option[RichDBObject] = None
      try {
        while (found.isEmpty && cursor.hasNext) {
          val obj = new RichDBObject(cursor.next)
          if (foundIt(obj)) {
            found = Some(obj)
          }
        }
      } finally {
        cursor.close()
      }
      found
    }
    def first(key: String): Option[RichDBObject] = top(key := ASC)
    def last(key: String): Option[RichDBObject] = top(key := DESC)
    def top(sorting: BsonIntProp, more: BsonIntProp*): Option[RichDBObject] = cursor.sort(obj(sorting, more: _*)).limit(1).nextOpt()
    def nextOpt(): Option[RichDBObject] = if (cursor.hasNext) Some(cursor.next) else None
    def foreach(f: RichDBObject ⇒ Unit) {
      try {
        while (cursor.hasNext) {
          f(new RichDBObject(cursor.next))
        }
      } finally {
        cursor.close()
      }
    }
    def getOrElse[T](f: RichDBObject ⇒ T, t: ⇒ T): T = {
      try {
        if (cursor.hasNext) {
          f(new RichDBObject(cursor.next))
        } else {
          t
        }
      } finally {
        cursor.close()
      }
    }
    def map[T](f: RichDBObject ⇒ T): Seq[T] = {
      val buffer = collection.mutable.Buffer[T]()
      foreach { dbo ⇒
        buffer += f(dbo)
      }
      buffer
    }

    def flatMap[T](f: RichDBObject ⇒ collection.GenTraversableOnce[T]): Seq[T] = {
      val buffer = collection.mutable.Buffer[T]()
      foreach { dbo ⇒
        f(dbo).foreach(buffer += _)
      }
      buffer
    }
  }

  implicit def prop2obj(prop: BsonProp) = obj(prop)

  private def iter2List[T](i: java.lang.Iterable[T])(implicit codec: Codec[T, BsonValue]) = {
    val list = new RichDBList
    var iter = i.iterator()
    while (iter.hasNext) {
      val t = codec.encode(iter.next)
      list += t.raw
    }
    list
  }

  def _id[T](value: T)(implicit codec: Codec[T, BsonValue]) = "_id" := value
  def $gt[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$gt" := value
  def $gte[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$gte" := value
  def $lt[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$lt" := value
  def $ne[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$ne" := value
  def $lte[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$lte" := value
  def $size(size: Int) = "$size" := size
  def $type(bsonType: BsonType) = "$type" := bsonType.typeNumber
  def $all[T](values: T*)(implicit codec: Codec[T, BsonValue]) = "$all" := arr(values.map(t ⇒ codec.encode(t)): _*)
  def $in[T](values: T*)(implicit codec: Codec[T, BsonValue]) = "$in" := arr(values.map(t ⇒ codec.encode(t)): _*)
  def $nin[T](values: T*)(implicit codec: Codec[T, BsonValue]) = "$nin" := arr(values.map(t ⇒ codec.encode(t)): _*)
  def $and[T](exprs: T*)(implicit codec: Codec[T, BsonValue]) = "$and" := arr(exprs.map(t ⇒ codec.encode(t)): _*)
  def $or[T](exprs: T*)(implicit codec: Codec[T, BsonValue]) = "$or" := arr(exprs.map(t ⇒ codec.encode(t)): _*)
  def $nor[T](exprs: T*)(implicit codec: Codec[T, BsonValue]) = "$nor" := arr(exprs.map(t ⇒ codec.encode(t)): _*)
  def $each[T](values: T*)(implicit codec: Codec[T, BsonValue]) = "$each" := arr(values.map(t ⇒ codec.encode(t)): _*)
  def $exists(exists: Boolean): BsonProp = "$exists" := exists
  def $set(props: Seq[BsonProp]) = "$set" := obj(props)
  def $set(prop: BsonProp, more: BsonProp*) = "$set" := obj(prop, more: _*)
  def $setOnInsert(props: Seq[BsonProp]) = "$setOnInsert" := obj(props)
  def $setOnInsert(prop: BsonProp, more: BsonProp*) = "$setOnInsert" := obj(prop, more: _*)
  def $unset(names: String*) = {
    val unsets = new RichDBObject
    names.foreach { name ⇒
      unsets.add(name := 1)
    }
    "$unset" := unsets
  }
  def $mod(modBy: Int, equalsTo: Int) = "$mod" := arr(IntCdc.encode(modBy), IntCdc.encode(equalsTo))
  def $not(props: Seq[BsonProp]) = "$not" := obj(props)
  def $not(prop: BsonProp, more: BsonProp*) = "$not" := obj(prop, more: _*)
  def $inc(props: Seq[BsonNumProp]) = "$inc" := obj(props)
  def $inc(prop: BsonNumProp, more: BsonNumProp*) = "$inc" := obj(prop, more: _*)
  def $push(props: Seq[BsonProp]) = "$push" := obj(props)
  def $push(prop: BsonProp, more: BsonProp*) = "$push" := obj(prop, more: _*)
  def $addToSet(prop: BsonProp) = "$addToSet" := obj(prop)
  def $pushAll(prop: BsonProp) = "$pushAll" := obj(prop)
  def $pop(prop: BsonIntProp): BsonProp = "$pop" := obj(prop)
  def $pop(name: String): BsonProp = "$pop" := obj(name := LAST)
  def $pull(prop: BsonProp) = "$pull" := obj(prop)
  def $elemMatch(props: Seq[BsonProp]) = "$elemMatch" := obj(props)
  def $elemMatch(prop: BsonProp, more: BsonProp*) = "$elemMatch" := obj(prop, more: _*)
  def $near(point: GeoPoint): BsonProp = {
    val geoDbo = geo2Dbo(point)
    val near = obj("$geometry" := geoDbo)
    if (point.radius > 0f) near("$maxDistance" := point.radius)
    "$near" := near
  }
  def $regex(regex: String, options: String = "") = {
    val dbo = obj("$regex" := regex)
    if (options.length != 0) dbo("$options" := options)
    dbo
  }
  def $where(jsExpr: String) = "$where" := jsExpr

  case class MapReduce(mapJS: String, reduceJS: String) {
    require(mapJS.trim.length > 0, "No JS code for `map` function")
    require(reduceJS.trim.length > 0, "No JS code for `reduce` function")
  }
  object MapReduce {
    private val FunctionMatcher = """^(map|reduce)\s*=\s*""".r.pattern
    private def compileCoffeeFunction(func: String) = {
      val js = coffeeCompiler.compile(func).trim()
      js.substring(1, js.length - 2)
    }
    def coffee(map: String, reduce: String): MapReduce = {
      val mapCoffee = compileCoffeeFunction(map)
      val reduceCoffee = compileCoffeeFunction(reduce)
      new MapReduce(mapCoffee, reduceCoffee)
    }

    /**
     * Takes an `InputStream` which is expected to have
     * 2 function declarations named `map` and `reduce`,
     * in CoffeeScript.
     */
    def brew(coffeescript: java.io.InputStream, encoding: String = "UTF-8"): MapReduce =
      brew(new java.io.InputStreamReader(coffeescript, encoding))

    /**
     * Takes a `Reader` which is expected to have
     * 2 function declarations named `map` and `reduce`,
     * in CoffeeScript.
     */
    def brew(coffeescript: java.io.Reader): MapReduce = {
      var active: Option[StringBuilder] = None
      val map = new StringBuilder
      val reduce = new StringBuilder
      val br = coffeescript match {
        case br: java.io.BufferedReader ⇒ br
        case r ⇒ new java.io.BufferedReader(r)
      }
      var line = br.readLine()
      while (line != null) {
        val m = FunctionMatcher.matcher(line)
        if (m.lookingAt()) {
          m.group(1) match {
            case "map" ⇒ active = Some(map)
            case "reduce" ⇒ active = Some(reduce)
          }
          line = m.replaceFirst("")
        }
        active.foreach(_ append line append '\n')
        line = br.readLine()
      }
      require(map.size > 0, "Map function not found. Must be named `map`.")
      require(reduce.size > 0, "Reduce function not found. Must be named `reduce`.")
      coffee(map.result, reduce.result)
    }
  }

  def parseJson(json: String): Any = com.mongodb.util.JSON.parse(json) match {
    case dbo: DBObject ⇒ enrich(dbo)
    case a ⇒ a
  }
  def parseJsonObject(json: String): Option[DBObject] = parseJson(json) match {
    case dbo: DBObject ⇒ Some(enrich(dbo))
    case a ⇒ None
  }

  private object Proxying {

    private val DefaultProxyMapping: Map[Class[_], Codec[_, BsonValue]] = Map(
      classOf[String] -> StrCdc,
      classOf[Double] -> DblCdc,
      classOf[java.lang.Double] -> DblCdc,
      classOf[Float] -> FltCdc,
      classOf[java.lang.Float] -> FltCdc,
      classOf[Long] -> LongCdc,
      classOf[java.lang.Long] -> LongCdc,
      classOf[Int] -> IntCdc,
      classOf[java.lang.Integer] -> IntCdc,
      classOf[Byte] -> ByteCdc,
      classOf[java.lang.Byte] -> ByteCdc,
      classOf[Boolean] -> BoolCdc,
      classOf[java.lang.Boolean] -> BoolCdc,
      classOf[Short] -> ShrtCdc,
      classOf[java.lang.Short] -> ShrtCdc,
      classOf[UUID] -> UUIDCdc,
      classOf[BigDecimal] -> BDCdc,
      classOf[ObjectId] -> OIDCdc,
      classOf[Array[Byte]] -> BACdc,
      classOf[Timestamp] -> TsCdc,
      classOf[java.util.Date] -> DateCdc,
      classOf[java.util.TimeZone] -> TzCdc,
      classOf[java.util.Locale] -> LocCdc,
      classOf[Number] -> BDCdc,
      classOf[EmailAddress] -> EmlCdc,
      classOf[Password] -> PwdCdc,
      classOf[GeoPoint] -> GeoCdc,
      classOf[java.net.URL] -> UrlCdc,
      classOf[java.net.URI] -> UriCdc)

    private def convertProxyValue(name: String, value: BsonField, asType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]) = mapping.get(asType) match {
      case Some(converter) ⇒ value.as(converter)
      case None ⇒
        if (asType.isInterface) {
          getProxy(value.as[DBObject], mapping)(ClassTag(asType))
        } else {
          import scuff._
          value match {
            case value: Value ⇒
              if (asType.isInstance(value.raw)) {
                value.raw
              } else {
                value.raw.coerceTo[Any](ClassTag(asType)).getOrElse(throw new InvalidValueTypeException(name, "Cannot convert %s to %s".format(value.raw, asType.getName)))
              }
            case _ ⇒ throw new UnavailableValueException(name, "Field %s is unavailable".format(name))
          }
        }
    }
    private def convertProxyOption(name: String, value: BsonField, optType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]) = mapping.get(optType) match {
      case Some(converter) ⇒ value.opt(converter)
      case None ⇒
        if (optType.isInterface) {
          value.opt[DBObject].map(getProxy(_, mapping)(ClassTag(optType)))
        } else {
          value match {
            case value: Value ⇒ Option(convertProxyValue(name, value, optType, mapping))
            case _ ⇒ None
          }
        }
    }
    private def convertProxySet(name: String, shouldBeSet: BsonField, setType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): Set[_] = mapping.get(setType) match {
      case Some(converter) ⇒ shouldBeSet.asSeq(converter).toSet
      case None ⇒
        if (setType.isInterface) {
          shouldBeSet.asSeq[DBObject].map(getProxy(_, mapping)(ClassTag(setType))).toSet
        } else {
          shouldBeSet match {
            case value: Value ⇒ value.raw match {
              case list: java.util.ArrayList[_] ⇒
                import collection.JavaConverters._
                list.asScala.map(elem ⇒ convertProxyValue(name, BsonField(elem), setType, mapping)).toSet
              case _ ⇒ Set(convertProxyValue(name, value, setType, mapping))
            }
            case _ ⇒ Set.empty
          }
        }
    }
    private def convertProxyMap(name: String, shouldBeMap: BsonField, mapType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): Map[String, _] = {
      import language.existentials
      shouldBeMap match {
        case value: Value ⇒ mapping.get(mapType) match {
          case Some(converter) ⇒ MapCdc(converter).decode(value)
          case None ⇒ value.raw match {
            case dbo: DBObject ⇒
              var map = Map.empty[String, Any]
              dbo.keys.foreach { key ⇒
                val value = convertProxyValue(key, dbo(key), mapType, mapping)
                map += key -> value
              }
              map
            case _ ⇒ throw new InvalidValueTypeException(name, "Cannot convert %s to Map[String, %s]".format(value.raw, mapType.getName))
          }
        }
        case _ ⇒ Map.empty
      }
    }

    private def convertProxySeq(name: String, shouldBeList: BsonField, seqType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): Seq[_] = mapping.get(seqType) match {
      case Some(converter) ⇒ shouldBeList.asSeq(converter)
      case None ⇒
        if (seqType.isInterface) {
          shouldBeList.asSeq[DBObject].map(getProxy(_, mapping)(ClassTag(seqType)))
        } else {
          shouldBeList match {
            case value: Value ⇒ value.raw match {
              case list: java.util.ArrayList[_] ⇒
                import collection.JavaConverters._
                list.asScala.map(elem ⇒ convertProxyValue(name, BsonField(elem), seqType, mapping))
              case _ ⇒ Seq(convertProxyValue(name, value, seqType, mapping))
            }
            case _ ⇒ Seq.empty
          }
        }
    }
    private def convertProxyList(name: String, shouldBeList: BsonField, seqType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): List[_] = mapping.get(seqType) match {
      case Some(converter) ⇒ shouldBeList.asList(converter)
      case None ⇒
        if (seqType.isInterface) {
          shouldBeList.asList[DBObject].map(getProxy(_, mapping)(ClassTag(seqType)))
        } else {
          shouldBeList match {
            case value: Value ⇒ value.raw match {
              case list: java.util.ArrayList[_] ⇒
                import collection.JavaConverters._
                list.asScala.map(elem ⇒ convertProxyValue(name, BsonField(elem), seqType, mapping)).toList
              case _ ⇒ List(convertProxyValue(name, value, seqType, mapping))
            }
            case _ ⇒ Nil
          }
        }
    }

    private def getGenericReturnClass(method: java.lang.reflect.Method) = {
      val typeArgs = method.getGenericReturnType().asInstanceOf[java.lang.reflect.ParameterizedType].getActualTypeArguments()
      typeArgs(typeArgs.length - 1).asInstanceOf[Class[_]]
    }

    def getProxy[T: ClassTag](dbo: RichDBObject, userMapping: Map[Class[_], Codec[_, BsonValue]]): T = {
      val fp = new Proxylicious[T]
      val mapping = DefaultProxyMapping ++ userMapping
      val proxy = fp.proxify {
        case (_, method, args) if args == null || args.length == 0 ⇒
          if (method.getName == "toString") {
            dbo.toJson()
          } else try {
            val value = dbo(method.getName)
            val rt = method.getReturnType
            if (rt == classOf[Option[_]]) {
              val rtt = getGenericReturnClass(method)
              convertProxyOption(method.getName, value, rtt, mapping)
            } else if (rt.isAssignableFrom(classOf[Seq[_]])) {
              val rtt = getGenericReturnClass(method)
              convertProxySeq(method.getName, value, rtt, mapping)
            } else if (rt == classOf[List[_]]) {
              val rtt = getGenericReturnClass(method)
              convertProxyList(method.getName, value, rtt, mapping)
            } else if (rt.isAssignableFrom(classOf[Set[_]])) {
              val rtt = getGenericReturnClass(method)
              convertProxySet(method.getName, value, rtt, mapping)
            } else if (rt.isAssignableFrom(classOf[Map[_, _]])) {
              val rtt = getGenericReturnClass(method)
              convertProxyMap(method.getName, value, rtt, mapping)
            } else {
              convertProxyValue(method.getName, value, rt, mapping)
            }
          } catch {
            case e: UnavailableValueException ⇒ throw e
            case e: Exception ⇒ throw new InvalidValueTypeException(method.getName, e.getMessage)
          }
        case (_, method, _) ⇒ throw new IllegalAccessException("Cannot proxy methods with arguments: " + method)
      }
      fp.sandwich(proxy, fp.EqualsHashCodeOverride)
    }

  }

}
