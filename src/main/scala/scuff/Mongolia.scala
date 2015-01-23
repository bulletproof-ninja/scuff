package scuff

import java.util.Date
import java.util.Locale
import java.util.TimeZone
import java.util.UUID
import scala.collection.GenTraversableOnce
import scala.language.implicitConversions
import scala.reflect.ClassTag
import org.bson.types._
import com.mongodb._
import scala.util.Try
import javax.script.ScriptEngine
import js.CoffeeScriptCompiler

/**
 * Convenience DSL for the MongoDB Java driver.
 */
object Mongolia {

  class UnavailableValueException(val fieldName: String, message: String)
    extends RuntimeException(message)
  class InvalidValueTypeException(val fieldName: String, cause: Throwable, message: String)
      extends RuntimeException(message, cause) {
    def this(fieldName: String, cause: Throwable) = this(fieldName, cause, cause.getMessage)
    def this(fieldName: String, message: String) = this(fieldName, null, message)
  }

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
      case n: Number ⇒ n.intValue
      case str: String ⇒ str.toInt
      case _ ⇒ org.bson.BSON.toInt(b.raw)
    }
  }
  implicit val JIntCdc = new Codec[Integer, BsonValue] {
    def encode(a: Integer): BsonValue = new Value(a)
    def decode(b: BsonValue): Integer = b.raw match {
      case n: Number ⇒ n.intValue
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
      case _ ⇒ throwCoercionException(b.raw, "Long")
    }
  }
  implicit val JLongCdc = new Codec[java.lang.Long, BsonValue] {
    def encode(a: java.lang.Long): BsonValue = new Value(a)
    def decode(b: BsonValue): java.lang.Long = b.raw match {
      case n: Number ⇒ n.longValue()
      case d: java.util.Date ⇒ d.getTime
      case s: String ⇒ s.toLong
      case _ ⇒ throwCoercionException(b.raw, "Long")
    }
  }
  implicit val DblCdc = new Codec[Double, BsonValue] {
    def encode(a: Double): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case n: Number ⇒ n.doubleValue()
      case s: String ⇒ s.toDouble
      case _ ⇒ throwCoercionException(b.raw, "Double")
    }
  }
  implicit val JDblCdc = new Codec[java.lang.Double, BsonValue] {
    def encode(a: java.lang.Double): BsonValue = new Value(a)
    def decode(b: BsonValue): java.lang.Double = b.raw match {
      case n: Number ⇒ n.doubleValue()
      case s: String ⇒ s.toDouble
      case _ ⇒ throwCoercionException(b.raw, "Double")
    }
  }
  implicit val FltCdc = new Codec[Float, BsonValue] {
    def encode(a: Float): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case n: Number ⇒ n.floatValue()
      case s: String ⇒ s.toFloat
      case _ ⇒ throwCoercionException(b.raw, "Float")
    }
  }
  private def throwCoercionException(from: Any, to: String) =
    throw new RuntimeException(s"Cannot coerce ${from.getClass.getName}($from) into $to")

  implicit val ShrtCdc = new Codec[Short, BsonValue] {
    def encode(a: Short): BsonValue = new Value(a)
    def decode(b: BsonValue): Short = b.raw match {
      case n: Number ⇒ n.shortValue()
      case s: String ⇒ s.toShort
      case _ ⇒ throwCoercionException(b.raw, "Short")
    }
  }
  implicit val ByteCdc = new Codec[Byte, BsonValue] {
    def encode(a: Byte): BsonValue = new Value(a)
    def decode(b: BsonValue): Byte = b.raw match {
      case n: Number ⇒ n.byteValue
      case s: String ⇒ s.toByte
      case _ ⇒ throwCoercionException(b.raw, "Short")
    }
  }
  implicit val BoolCdc = new Codec[Boolean, BsonValue] {
    val TrueStrings = Set("1", "true", "on", "yes", "y")
    def encode(a: Boolean): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case b: java.lang.Boolean ⇒ b.booleanValue
      case i: Int ⇒ IntCdc.decode(b) != 0
      case s: String ⇒ TrueStrings.contains(s.toLowerCase)
      case _ ⇒ throwCoercionException(b.raw, "Boolean")
    }
  }
  implicit val BACdc = new Codec[Array[Byte], BsonValue] {
    def encode(a: Array[Byte]): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case arr: Array[Byte] ⇒ arr
      case bin: Binary ⇒ bin.getData
      case _ ⇒ throwCoercionException(b.raw, "Array[Byte]")
    }
  }
  implicit val IACdc = new Codec[Array[Int], BsonValue] {
    def encode(a: Array[Int]): BsonValue = new Value(arr(a: _*))
    def decode(b: BsonValue): Array[Int] = b.raw match {
      case arr: Array[Int] ⇒ arr
      case list: java.util.List[_] ⇒
        val array = new Array[Int](list.size)
        val iter = list.iterator()
        var idx = 0
        while (iter.hasNext) {
          iter.next match {
            case n: Number ⇒ array(idx) = n.intValue
            case v ⇒ throwCoercionException(v, "Int")
          }
          idx += 1
        }
        array
      case _ ⇒ throwCoercionException(b.raw, "Array[Int]")
    }
  }
  implicit val LACdc = new Codec[Array[Long], BsonValue] {
    def encode(a: Array[Long]): BsonValue = new Value(arr(a: _*))
    def decode(b: BsonValue): Array[Long] = b.raw match {
      case arr: Array[Long] ⇒ arr
      case list: java.util.List[_] ⇒
        val array = new Array[Long](list.size)
        val iter = list.iterator()
        var idx = 0
        while (iter.hasNext) {
          iter.next match {
            case n: Number ⇒ array(idx) = n.longValue
            case v ⇒ throwCoercionException(v, "Long")
          }
          idx += 1
        }
        array
      case _ ⇒ throwCoercionException(b.raw, "Array[Long]")
    }
  }
  implicit val DACdc = new Codec[Array[Double], BsonValue] {
    def encode(a: Array[Double]): BsonValue = new Value(arr(a: _*))
    def decode(b: BsonValue): Array[Double] = b.raw match {
      case arr: Array[Double] ⇒ arr
      case list: java.util.List[_] ⇒
        val array = new Array[Double](list.size)
        val iter = list.iterator()
        var idx = 0
        while (iter.hasNext) {
          iter.next match {
            case n: Number ⇒ array(idx) = n.doubleValue
            case v ⇒ throwCoercionException(v, "Double")
          }
          idx += 1
        }
        array
      case _ ⇒ throwCoercionException(b.raw, "Array[Double]")
    }
  }
  implicit val BinCdc = new Codec[Binary, BsonValue] {
    def encode(a: Binary): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case arr: Array[Byte] ⇒ new Binary(arr)
      case bin: Binary ⇒ bin
      case _ ⇒ throwCoercionException(b.raw, "Binary")
    }
  }
  implicit val DateCdc = new Codec[Date, BsonValue] {
    def encode(a: Date): BsonValue = new Value(a)
    def decode(b: BsonValue): Date = b.raw match {
      case n: Number ⇒ new Date(n.longValue)
      case ts: Timestamp ⇒ new Date(ts.asMillis)
      case d: Date ⇒ d
      case oid: ObjectId ⇒ oid.getDate
      case _ ⇒ throwCoercionException(b.raw, "Date")
    }
  }
  implicit val TsCdc = new Codec[Timestamp, BsonValue] {
    def encode(a: Timestamp): BsonValue = new Value(a)
    def decode(b: BsonValue) = b.raw match {
      case n: Number ⇒ new Timestamp(n.longValue)
      case ts: Timestamp ⇒ ts
      case d: Date ⇒ new Timestamp(d)
      case oid: ObjectId ⇒ new Timestamp(oid.getDate.getTime)
      case str: String ⇒ Timestamp.parseISO(str).get
      case _ ⇒ throwCoercionException(b.raw, "Timestamp")
    }
  }
  implicit val OIDCdc = new Codec[ObjectId, BsonValue] {
    def encode(a: ObjectId): BsonValue = new Value(a)
    def decode(b: BsonValue) = decode(b.raw)
    def decode(any: Any) = any match {
      case oid: ObjectId => oid
      case arr: Array[Byte] if arr.length == 12 => new ObjectId(arr)
      case str: String if ObjectId.isValid(str) => new ObjectId(str)
      case _ ⇒ throwCoercionException(any, "ObjectId")
    }
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
      case _ ⇒ throwCoercionException(b.raw, "UUID")
    }
  }
  implicit val PwdCdc = new Codec[Password, BsonValue] {
    def encode(a: Password): BsonValue = {
      val dbo = obj("digest" := a.digest, "algo" := a.algorithm)
      if (a.salt.length > 0) dbo.add("salt" := a.salt)
      if (a.workFactor > 1) dbo.add("work" := a.workFactor)
      new Value(dbo)
    }
    def decode(b: BsonValue) = b.raw match {
      case obj: DBObject ⇒
        val dbo = enrich(obj)
        new Password(dbo("digest").as[Array[Byte]], dbo("algo").as[String], dbo("salt").opt[Array[Byte]].getOrElse(Array.empty), dbo("work").opt[Int].getOrElse(1))
      case _ ⇒ throwCoercionException(b.raw, "Password")
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
      case _ ⇒ throwCoercionException(b.raw, "BigDecimal")
    }
  }
  implicit def EnumCdc[T <: Enum[T]](implicit tag: ClassTag[T]) = new Codec[T, BsonValue] {
    private def constants = tag.runtimeClass.getEnumConstants().asInstanceOf[Array[T]]
    def encode(e: T): BsonValue = new Value(e.ordinal)
    def decode(b: BsonValue): T = b.raw match {
      case i: Int => constants(i)
      case name: String => findByName(name)
      case n: Number => constants(n.intValue)
      case _ ⇒ throwCoercionException(b.raw, tag.runtimeClass.getName)
    }
    @annotation.tailrec
    private def findByName(name: String, idx: Int = 0): T = {
      if (idx == constants.length) {
        throwCoercionException(name, tag.runtimeClass.getName)
      } else {
        val enum = constants(idx)
        if (enum.name == name) {
          enum
        } else {
          findByName(name, idx + 1)
        }
      }
    }
  }

  implicit def SEnumCdc[T <: Enumeration](implicit enum: T) = new Codec[T#Value, BsonValue] {
    def encode(e: T#Value): BsonValue = new Value(e.id)
    def decode(b: BsonValue): T#Value = b.raw match {
      case i: Int => enum(i)
      case name: String => enum.withName(name)
      case n: Number => enum(n.intValue)
      case _ ⇒ throwCoercionException(b.raw, enum.getClass.getName)
    }
  }

  private def geo2Dbo(gp: GeoPoint): BsonObject = obj("type" := "Point", "coordinates" := arr(gp.longitude: Double, gp.latitude: Double))
  implicit def GeoPointCdc: Codec[GeoPoint, BsonValue] = GeoCdc
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
      case _ ⇒ throwCoercionException(b.raw, "GeoPoint")
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
      case l: BsonList ⇒ l
      case _ ⇒ enrich(a)
    }
    def decode(b: BsonValue): DBObject = b.raw match {
      case list: BasicDBList ⇒ list
      case list: org.bson.LazyDBList => list
      case dbo: DBObject ⇒ dbo.enrich: DBObject
      case _ ⇒ throwCoercionException(b.raw, "DBObject")
    }
  }
  implicit def BsonObjectCdc: Codec[BsonObject, BsonValue] = RDboCdc
  private[this] val RDboCdc = new Codec[BsonObject, BsonValue] {
    def encode(a: BsonObject): BsonValue = a
    def decode(b: BsonValue): BsonObject = b.raw match {
      case dbo: BsonObject ⇒ dbo
      case dbo: DBObject ⇒ dbo.enrich
      case _ ⇒ throwCoercionException(b.raw, "BsonObject")
    }
  }

  implicit def MapCdc[T](implicit codec: Codec[T, BsonValue]) = new Codec[Map[String, T], BsonValue] {
    def encode(a: Map[String, T]): BsonValue = {
      val dbo = new BsonObject
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
    val list = new BsonList
    seq.foreach { t ⇒
      val value = if (t == null) null else {
        val bson = codec.encode(t)
        if (bson == null) null else bson.raw
      }
      list += value
    }
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

  def obj(ignoreNulls: Boolean = false, ignoreEmpty: Boolean = false): BsonObject = new BsonObject(ignoreNulls = ignoreNulls, ignoreEmpty = ignoreEmpty)
  def obj(props: Seq[BsonProp]): BsonObject = {
    val map = new BsonObject
    props.foreach(p ⇒ if (map.put(p.key, p.raw) != null) throw new IllegalArgumentException("Field \"%s\" occurs multiple times".format(p.key)))
    map
  }
  def obj(head: BsonProp, tail: BsonProp*): BsonObject = {
    val map = new BsonObject(new BasicDBObject(head.key, head.raw))
    tail.foreach(p ⇒ if (map.put(p.key, p.raw) != null) throw new IllegalArgumentException("Field \"%s\" occurs multiple times".format(p.key)))
    map
  }
  def arr(values: BsonValue*): BsonList = seq2bsonlist(values)(Codec.noop)
  def arr[T](values: T*)(implicit codec: Codec[T, BsonValue]): BsonList = seq2bsonlist(values)
  def obj[T](map: collection.Map[String, T])(implicit codec: Codec[T, BsonValue]): BsonObject = new BsonObject().add(map)

  implicit def enrich(poor: DBObject) = poor match {
    case rich: BsonObject ⇒ rich
    case _ ⇒ new BsonObject(poor)
  }
  implicit def impoverish(rich: BsonObject) = rich.impoverish
  implicit def impoverish(rich: DocCollection) = rich.impoverish
  implicit def impoverish(rich: BsonCursor) = rich.impoverish
  implicit def impoverish(rich: Database) = rich.impoverish

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
      case _ ⇒ throwCoercionException(any, "java.util.List")
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
      case Some(name) ⇒ throw new UnavailableValueException(name, s"""Field "$name" is null""")
    }
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asSeq[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asList[T](implicit codec: Codec[T, BsonValue]) = Nil
  }
  final class Missing private[Mongolia] (fieldName: Option[String]) extends BsonField {
    def opt[T](implicit codec: Codec[T, BsonValue]) = None
    def as[T](implicit codec: Codec[T, BsonValue]): T = fieldName match {
      case None ⇒ throw new UnavailableValueException("", "Field does not exist")
      case Some(name) ⇒ throw new UnavailableValueException(name, s"""Field "$name" does not exist""")
    }
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asSeq[T](implicit codec: Codec[T, BsonValue]) = IndexedSeq.empty
    def asList[T](implicit codec: Codec[T, BsonValue]) = Nil
  }
  final class Value private[Mongolia] (val raw: Any) extends BsonField with BsonValue {
    override def toString() = "%s = %s".format(raw.getClass.getName, raw)
    def opt[T](implicit codec: Codec[T, BsonValue]): Option[T] = Option(codec.decode(this))
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

  implicit final class Database(val underlying: DB) extends AnyVal {
    def impoverish = underlying
    def apply(collection: String, wc: WriteConcern = null) = {
      val dbColl = underlying.getCollection(collection)
      if (wc != null) dbColl.setWriteConcern(wc)
      new DocCollection(dbColl)
    }
    def enrich: Database = this
  }

  implicit final class DocCollection(val underlying: DBCollection) extends AnyVal {
    implicit def impoverish = underlying
    private def SAFE = underlying.getWriteConcern.getWObject.asInstanceOf[Any] match {
      case w: Int if w < WriteConcern.SAFE.getW ⇒ WriteConcern.SAFE
      case _ ⇒ underlying.getWriteConcern
    }
    def safeInsert(dbo: DBObject, more: DBObject*) = {
      if (more.isEmpty) {
        underlying.insert(dbo, SAFE)
      } else {
        val all = Array(dbo) ++ more
        underlying.insert(all, SAFE)
      }
    }
    def safeInsert(dbos: Iterable[DBObject]) = underlying.insert(dbos.toArray, SAFE)
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
    private def includeFields(fields: Seq[String]): DBObject = if (fields.isEmpty) null else obj(fields.map(_ := INCLUDE))
    def updateAndReturn(query: DBObject, upd: DBObject, returnFields: String*): Option[DBObject] = Option(underlying.findAndModify(query, includeFields(returnFields), null, false, upd, true, false))
    def upsertAndReturn(query: DBObject, upd: DBObject, returnFields: String*): DBObject = underlying.findAndModify(query, includeFields(returnFields), null, false, upd, true, true)
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

    def createIndex(key: String): Unit = underlying.createIndex(obj(key := ASC))
    def createIndex(key: String, idxType: String): Unit = underlying.createIndex(obj(key := idxType))
    def createIndex(keyHead: BsonIntProp, keyTail: BsonIntProp*): Unit = underlying.createIndex(obj(keyHead, keyTail: _*))
    def createUniqueIndex(key: String): Unit = underlying.createIndex(obj(key := ASC), obj("unique" := true))
    def createHashedIndex(key: String): Unit = underlying.createIndex(obj(key := "hashed"))
    def createTextIndex(fields: Set[String], langField: String = null, defaultLang: Locale = null, indexName: String = null): Unit = {
      require(fields.nonEmpty, "Must have at least one field to index on")
      val keys = fields.foldLeft(obj()) {
        case (keys, field) => keys(field := "text")
      }
      val defLang = Option(defaultLang).filterNot(_ == Locale.ROOT).map(_.toLanguageTag).getOrElse("none")
      val options = obj("default_language" := defLang)
      Option(langField).foreach { langField =>
        options("language_override" := langField)
      }
      Option(indexName).foreach(name => options("name" := name))
      underlying.createIndex(keys, options)
    }
    def createSparseIndex(key: String): Unit = underlying.createIndex(obj(key := ASC), obj("sparse" := true))
    def createUniqueSparseIndex(key: String): Unit = underlying.createIndex(obj(key := ASC), obj("sparse" := true, "unique" := true))
    def createUniqueIndex(keyHead: BsonIntProp, keyTail: BsonIntProp*): Unit = underlying.createIndex(obj(keyHead, keyTail: _*), obj("unique" := true))
    def findOne(anyRef: Object) = anyRef match {
      case key: DBObject ⇒ underlying.findOne(key)
      case prop: BsonProp ⇒ underlying.findOne(obj(prop))
      case value: BsonValue ⇒ underlying.findOne(_id(value))
      case _ ⇒ underlying.findOne(anyRef)
    }
    def findOpt(key: DBObject, fields: DBObject = null) = underlying.findOne(key, fields) match {
      case null ⇒ None
      case doc ⇒ Some(doc.enrich)
    }
  }

  private val serializers = new ResourcePool(com.mongodb.util.JSONSerializers.getStrict)

  private val base64 = Base64.Custom('+', '/', withPadding = true, paddingChar = '=')

  /**
   * Much faster and more compact serialization,
   * and more importantly *correct* JSON, i.e.
   * `NaN` is translated to `null`.
   */
  def toJson(dbo: DBObject, sb: java.lang.StringBuilder = new java.lang.StringBuilder(128)): java.lang.StringBuilder = {
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
        case r: BsonObject ⇒ appendRef(r.impoverish)
        case f: Float ⇒ if (java.lang.Float.isNaN(f) || java.lang.Float.isInfinite(f)) sb append "null" else sb append f
        case _ ⇒ appendRef(any.asInstanceOf[AnyRef])
      }
      def appendRef(anyRef: AnyRef): Unit = {
        import collection.JavaConverters._
        anyRef match {
          case m: java.util.Map[_, _] ⇒ appendMap(m)
          case l: java.lang.Iterable[_] ⇒ appendList(l.asScala)
          case t: collection.GenTraversableOnce[_] ⇒ appendList(t)
          case _ ⇒ serializers.borrow(_.serialize(anyRef, sb))
        }
      }

    append(dbo)
    sb
  }

  final class BsonObject(private val underlying: DBObject = new BasicDBObject, ignoreNulls: Boolean = false, ignoreEmpty: Boolean = false) extends DBObject with BsonValue {
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
    def getOrUpdate[T](key: String, ctor: => T)(implicit codec: Codec[T, BsonValue]): T = {
      apply(key) match {
        case v: Value => codec.decode(v)
        case _ =>
          val t = ctor
          apply(key := t)
          t
      }
    }

    /**
     * Modify, mutable, object.
     * @param key The key name
     * @param initValue The value to initialize with, if key is missing (or null)
     * @param updater The updater function
     */
    def modify[T <: AnyRef](key: String, initValue: => T)(modifier: T => Unit)(implicit codec: Codec[T, BsonValue]): Unit = {
      val currT = apply(key) match {
        case v: Value => codec.decode(v)
        case _ =>
          val newT = initValue
          apply(key := newT)
          newT
      }
      modifier(currT)
    }

    /**
     * Update, immutable, value.
     * @param key The key name
     * @param initValue Optional. The value to initialize with, if key is missing or null.
     * If not provided, no update will happen if key is missing or null
     * @param updater The updater function
     */
    def update[T](key: String, initValue: T = null)(updater: T => T)(implicit codec: Codec[T, BsonValue]): Unit = {
      val currT = apply(key) match {
        case v: Value => codec.decode(v)
        case _ => initValue
      }
      if (currT != null) {
        apply(key := updater(currT))
      }
    }

    def replace(prop: BsonProp): BsonField = {
      val current = apply(prop.key)
      apply(prop)
      current
    }
    def toMap = underlying.toMap
    def removeField(key: String) = underlying.removeField(key)
    @deprecated(message = "Deprecated", since = "Don't care")
    def containsKey(s: String) = underlying.containsKey(s)
    def containsField(s: String) = underlying.containsField(s)
    def keySet = underlying.keySet
    def add[T](map: collection.Map[String, T])(implicit codec: Codec[T, BsonValue]): BsonObject = {
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
    def ignoreNulls(ignore: Boolean): BsonObject = if (ignore == this.ignoreNulls) this else new BsonObject(underlying, ignore, this.ignoreEmpty)
    /**
     * Ignore empty arrays, i.e. don't put them into doc.
     */
    def ignoreEmpty(ignore: Boolean): BsonObject = if (ignore == this.ignoreEmpty) this else new BsonObject(underlying, this.ignoreNulls, ignore)
    def keys: Iterable[String] = underlying.keySet.asScala
    /**
     * Much faster and more compact serialization,
     * and more importantly *correct* JSON.
     */
    def toJson(sb: java.lang.StringBuilder): java.lang.StringBuilder = Mongolia.toJson(underlying, sb)
    def toJson(): String = Mongolia.toJson(underlying).toString
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
      if (tag.runtimeClass.isInterface) {
        Proxying.getProxy(this, mapping)
      } else throw new IllegalArgumentException(s"${tag.runtimeClass} must be an interface")

    def isEmpty = underlying match {
      case m: java.util.Map[_, _] ⇒ m.isEmpty
      case _ ⇒ underlying.keySet.isEmpty
    }
    def prop(key: String): BsonProp = new BsonProp(key, new Value(underlying.get(key)))
    def asSeq[T](implicit codec: Codec[T, BsonValue]) = new Value(underlying).asSeq[T]
    def asSeqOfOption[T](implicit codec: Codec[T, BsonValue]) = new Value(underlying).asSeqOfOption[T]
    def add(head: BsonProp, tail: BsonProp*): BsonObject = {
      this.put(head.key, head.raw)
      tail.foreach { prop ⇒
        this.put(prop.key, prop.raw)
      }
      this
    }
    def add(props: Seq[BsonProp]): BsonObject = {
      props.foreach { prop ⇒
        this.put(prop.key, prop.raw)
      }
      this
    }
    def enrich = this
    def impoverish = underlying
    def _id = underlying.get("_id") match {
      case null ⇒ throw new IllegalArgumentException("Field \"_id\" is missing")
      case oid: ObjectId ⇒ oid
      case any ⇒
        import language.reflectiveCalls
        OIDCdc.decode(any)
    }
    def apply(key: String): BsonField = BsonField(getAs[Any](key), underlying, key)
    def apply(head: BsonProp, tail: BsonProp*): BsonObject = add(head, tail: _*)
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

    def map[T](f: BsonObject ⇒ T): T = f(this)

  }

  final class BsonList extends BasicDBList with BsonValue {
    def raw = this
    /**
     * Much faster and more compact serialization,
     * and more importantly *correct* JSON, i.e.
     * `NaN` is translated to `null`.
     */
    def toJson(): String = Mongolia.toJson(this).toString
    def +=(any: Any) = add(any.asInstanceOf[Object])
  }

  implicit final class BsonCursor(val cursor: DBCursor) extends AnyVal {
    def impoverish = cursor
    override def toString = map(_.toString).mkString("[", ",", "]")
    def toSeq() = map(d ⇒ d)
    def find(foundIt: BsonObject ⇒ Boolean): Option[BsonObject] = {
      var found: Option[BsonObject] = None
      try {
        while (found.isEmpty && cursor.hasNext) {
          val obj = new BsonObject(cursor.next)
          if (foundIt(obj)) {
            found = Some(obj)
          }
        }
      } finally {
        cursor.close()
      }
      found
    }
    def first(key: String): Option[BsonObject] = top(key := ASC)
    def last(key: String): Option[BsonObject] = top(key := DESC)
    def top(sorting: BsonIntProp, more: BsonIntProp*): Option[BsonObject] = try {
      val cursor = this.cursor.sort(obj(sorting, more: _*)).limit(1)
      if (cursor.hasNext) Some(cursor.next) else None
    } finally {
      cursor.close()
    }
    def foreach(f: BsonObject ⇒ Unit) {
      try {
        while (cursor.hasNext) {
          f(new BsonObject(cursor.next))
        }
      } finally {
        cursor.close()
      }
    }
    def getOrElse[T](f: BsonObject ⇒ T, t: ⇒ T): T = {
      try {
        if (cursor.hasNext) {
          f(new BsonObject(cursor.next))
        } else {
          t
        }
      } finally {
        cursor.close()
      }
    }
    def map[T](f: BsonObject ⇒ T): Seq[T] = {
      val buffer = collection.mutable.Buffer[T]()
      foreach { dbo ⇒
        buffer += f(dbo)
      }
      buffer
    }

    def flatMap[T](f: BsonObject ⇒ collection.GenTraversableOnce[T]): Seq[T] = {
      val buffer = collection.mutable.Buffer[T]()
      foreach { dbo ⇒
        f(dbo).foreach(buffer += _)
      }
      buffer
    }
    def foldLeft[T](initial: T)(f: (T, BsonObject) => T): T = {
      var t = initial
      foreach { dbo =>
        t = f(t, dbo)
      }
      t
    }
  }

  implicit def prop2obj(prop: BsonProp) = obj(prop)

  private def iter2List[T](i: java.lang.Iterable[T])(implicit codec: Codec[T, BsonValue]) = {
    val list = new BsonList
    var iter = i.iterator()
    while (iter.hasNext) {
      val t = codec.encode(iter.next)
      list += t.raw
    }
    list
  }

  def _id[T](value: T)(implicit codec: Codec[T, BsonValue]) = obj("_id" := value)
  private def escapeSearchTerm(term: String, exclude: Boolean): String = {
    val cleanTerm = if (term startsWith "-") term.substring(1) else term
    val escapedTerm =
      if (term.contains(' ')) {
        "\"" + term + "\""
      } else term
    if (exclude) {
      "-" concat term
    } else {
      term
    }
  }
  def $text(terms: Set[String], not: Set[String] = Set.empty, lang: Locale = null, limit: Option[Int] = None, filter: DBObject = null, project: DBObject = null) = {
    val escapedTerms = terms.iterator.map(escapeSearchTerm(_, exclude = false))
    val escapedNot = not.iterator.map(escapeSearchTerm(_, exclude = true))
    val allTerms = escapedTerms ++ escapedNot
    val textDoc = obj("$search" := allTerms.mkString(" "))
    val langTag = if (lang != null) lang.toLanguageTag else "none"
    textDoc("$language" := langTag)
    limit.foreach(limit => textDoc("limit" := limit))
    if (filter != null && !filter.isEmpty) {
      textDoc("filter" := filter)
    }
    if (project != null && !project.isEmpty) {
      textDoc("project" := project)
    }
    "$text" := textDoc
  }
  def $gt[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$gt" := value
  def $gte[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$gte" := value
  def $lt[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$lt" := value
  def $ne[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$ne" := value
  def $lte[T](value: T)(implicit codec: Codec[T, BsonValue]) = "$lte" := value
  def $size(size: Int) = "$size" := size
  def $type(bsonType: BsonType) = "$type" := bsonType.typeNumber
  def $all[T](values: T*)(implicit codec: Codec[T, BsonValue]) = "$all" := arr(values: _*)
  def $in[T](values: T*)(implicit codec: Codec[T, BsonValue]) = "$in" := arr(values: _*)
  def $nin[T](values: T*)(implicit codec: Codec[T, BsonValue]) = "$nin" := arr(values: _*)
  def $and[T](exprs: T*)(implicit codec: Codec[T, BsonValue]) = "$and" := arr(exprs: _*)
  def $or[T](exprs: T*)(implicit codec: Codec[T, BsonValue]) = "$or" := arr(exprs: _*)
  def $nor[T](exprs: T*)(implicit codec: Codec[T, BsonValue]) = "$nor" := arr(exprs: _*)
  def $each[T](values: Seq[T])(implicit codec: Codec[T, BsonValue]) = "$each" := arr(values: _*)
  def $each[T](value: T, more: T*)(implicit codec: Codec[T, BsonValue]) = "$each" := arr((value :: more.toList): _*)
  def $exists(exists: Boolean): BsonProp = "$exists" := exists
  def $set(props: Seq[BsonProp]): BsonProp = "$set" := obj(props)
  def $set(prop: BsonProp, more: BsonProp*): BsonProp = "$set" := obj(prop, more: _*)
  def $setOnInsert(props: Seq[BsonProp]): BsonProp = "$setOnInsert" := obj(props)
  def $setOnInsert(prop: BsonProp, more: BsonProp*): BsonProp = "$setOnInsert" := obj(prop, more: _*)
  def $unset(names: String*) = {
    val unsets = new BsonObject
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
  def $addToSet(props: Seq[BsonProp]) = "$addToSet" := obj(props)
  def $addToSet(prop: BsonProp, more: BsonProp*) = "$addToSet" := obj(prop, more: _*)
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
  def $currentDate(field: String, otherFields: String*): BsonProp = {
    val cdDoc = obj(field := true)
    if (otherFields.nonEmpty) {
      cdDoc.add(otherFields.map(_ := true))
    }
    "$currentDate" := cdDoc
  }
  def $currentDate(head: BsonProp, tail: BsonProp*): BsonProp = "$currentDate" := obj(head, tail: _*)

  case class MapReduce(mapJS: String, reduceJS: String) {
    require(mapJS.trim.length > 0, "No JS code for `map` function")
    require(reduceJS.trim.length > 0, "No JS code for `reduce` function")
  }
  object MapReduce {
    private val FunctionMatcher = """^(map|reduce)\s*=\s*""".r.pattern
    private def compileCoffeeFunction(func: String, compiler: CoffeeScriptCompiler) = {
      val js = compiler.compile(func).trim()
      js.substring(1, js.length - 2)
    }
    def coffee(map: String, reduce: String)(implicit compiler: CoffeeScriptCompiler): MapReduce = {
      val mapCoffee = compileCoffeeFunction(map, compiler)
      val reduceCoffee = compileCoffeeFunction(reduce, compiler)
      new MapReduce(mapCoffee, reduceCoffee)
    }

    /**
     * Takes an `InputStream` which is expected to have
     * 2 function declarations named `map` and `reduce`,
     * in CoffeeScript.
     */
    def brew(coffeescript: java.io.InputStream, encoding: String = "UTF-8")(implicit compiler: CoffeeScriptCompiler): MapReduce =
      brew(new java.io.InputStreamReader(coffeescript, encoding))

    /**
     * Takes a `Reader` which is expected to have
     * 2 function declarations named `map` and `reduce`,
     * in CoffeeScript.
     */
    def brew(coffeescript: java.io.Reader)(implicit compiler: CoffeeScriptCompiler): MapReduce = {
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
  def parseJsonObject(json: String): Option[DBObject] = json match {
    case null ⇒ None
    case json ⇒
      parseJson(json) match {
        case dbo: DBObject ⇒ Some(enrich(dbo))
        case _ ⇒ None
      }
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
      classOf[Array[Int]] -> IACdc,
      classOf[Array[Long]] -> LACdc,
      classOf[Array[Double]] -> DACdc,
      classOf[Array[String]] -> ArrayListCdc[String],
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

    private def convertProxyValue(name: String, value: BsonField, asType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]) =
      mapping.get(asType) match {
        case Some(converter) ⇒
          value match {
            case value: Value => value.as(converter)
            case _ => null
          }
        case None ⇒
          import scuff._
          value match {
            case value: Value ⇒
              if (asType.isInstance(value.raw)) {
                value.raw
              } else if (isProxyable(asType)) {
                getProxy(value.as[DBObject], mapping)(ClassTag[Any](asType))
              } else {
                new ScuffAny(value.raw).coerceTo[Any](ClassTag(asType)).getOrElse(throw new InvalidValueTypeException(name, s"Cannot convert ${value.raw.getClass.getName}(${value.raw}) to ${asType.getName}"))
              }
            case _ => null
          }
      }
    private def convertProxySet(name: String, shouldBeSet: BsonField, setType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): Set[_] = mapping.get(setType) match {
      case Some(converter) ⇒ shouldBeSet.asSeq(converter).toSet
      case None ⇒
        if (setType.isInterface) {
          shouldBeSet.asSeq[DBObject].map(getProxy(_, mapping)(ClassTag[Any](setType))).toSet
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
    private def convertProxyMap(name: String, shouldBeMap: BsonField, valType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): Map[String, _] = {
      shouldBeMap match {
        case value: Value ⇒
          mapping.get(valType) match {
            case Some(valConv) ⇒ MapCdc(valConv).decode(value)
            case None ⇒ value.raw match {
              case dbo: DBObject ⇒
                var map = Map.empty[String, Any]
                dbo.keys.foreach { key ⇒
                  val value = convertProxyValue(key, dbo(key), valType, mapping)
                  map += key -> value
                }
                map
              case e: Exception ⇒ throw new InvalidValueTypeException(name, e, s"Cannot convert ${value.raw.getClass.getName}(${value.raw}) to Map[String, ${valType.getName}]")
            }
          }
        case _ ⇒ Map.empty
      }
    }

    private def convertProxySeq(name: String, shouldBeList: BsonField, seqType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): IndexedSeq[_] = mapping.get(seqType) match {
      case Some(converter) ⇒ shouldBeList.asSeq(converter)
      case None ⇒
        if (seqType.isInterface) {
          shouldBeList.asSeq[DBObject].map(getProxy(_, mapping)(ClassTag[Any](seqType)))
        } else {
          shouldBeList match {
            case value: Value ⇒ value.raw match {
              case list: java.util.List[_] ⇒
                import collection.JavaConversions._
                list.toArray().map(elem ⇒ convertProxyValue(name, BsonField(elem), seqType, mapping))
              case _ ⇒ IndexedSeq(convertProxyValue(name, value, seqType, mapping))
            }
            case _ ⇒ IndexedSeq.empty
          }
        }
    }
    private def convertProxyList(name: String, shouldBeList: BsonField, seqType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]]): List[_] = mapping.get(seqType) match {
      case Some(converter) ⇒ shouldBeList.asList(converter)
      case None ⇒
        if (seqType.isInterface) {
          shouldBeList.asList[DBObject].map(getProxy(_, mapping)(ClassTag[Any](seqType)))
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

    private def getGenericReturnClass(method: java.lang.reflect.Method, depth: Int) = {
        def getGenericType(t: java.lang.reflect.Type, depth: Int): Class[_] = {
          val typeArgs = t match {
            case pt: java.lang.reflect.ParameterizedType ⇒ pt.getActualTypeArguments()
          }
          typeArgs(typeArgs.length - 1) match {
            case pt: java.lang.reflect.ParameterizedType ⇒
              if (depth == 0) {
                pt.getRawType.asInstanceOf[Class[_]]
              } else {
                getGenericType(pt, depth - 1)
              }
            case clz: Class[_] ⇒ clz
          }
        }
      getGenericType(method.getGenericReturnType, depth)
    }

    private def extractValue(valueName: String, method: java.lang.reflect.Method, value: BsonField,
      expectedType: Class[_], mapping: Map[Class[_], Codec[_, BsonValue]], depth: Int = 0): Any = try {
      if (expectedType == classOf[Object]) {
        convertProxyValue(valueName, value, expectedType, mapping)
      } else if (expectedType == classOf[Option[_]]) {
        val rtt = getGenericReturnClass(method, depth)
        Option(extractValue(valueName, method, value, rtt, mapping, depth + 1))
      } else if (expectedType.isAssignableFrom(classOf[IndexedSeq[_]])) {
        val rtt = getGenericReturnClass(method, depth)
        convertProxySeq(valueName, value, rtt, mapping)
      } else if (expectedType == classOf[List[_]]) {
        val rtt = getGenericReturnClass(method, depth)
        convertProxyList(valueName, value, rtt, mapping)
      } else if (expectedType.isAssignableFrom(classOf[Set[_]])) {
        val rtt = getGenericReturnClass(method, depth)
        convertProxySet(valueName, value, rtt, mapping)
      } else if (expectedType.isAssignableFrom(classOf[Map[_, _]])) {
        val rtt = getGenericReturnClass(method, depth)
        convertProxyMap(valueName, value, rtt, mapping)
      } else {
        convertProxyValue(valueName, value, expectedType, mapping)
      }
    } catch {
      case e: UnavailableValueException ⇒ throw e
      case e: Exception ⇒ throw new InvalidValueTypeException(valueName, e)
    }

    def getProxy[T: ClassTag](dbo: BsonObject, userMapping: Map[Class[_], Codec[_, BsonValue]]): T = {
        def checkNotNull(any: Any, name: String): Any = any match {
          case null => throw new UnavailableValueException(name, s"$name is null")
          case _ => any
        }
      val fp = new Proxylicious[T]
      val mapping = DefaultProxyMapping ++ userMapping
      val proxy = fp.proxify {
        case (_, method, args) if args == null || args.length == 0 ⇒
          if (method.getName == "toString") {
            dbo.toJson()
          } else {
            checkNotNull(extractValue(method.getName, method, dbo(method.getName), method.getReturnType, mapping), method.getName)
          }
        case (_, method, args) if args != null && args.length == 1 && args(0).isInstanceOf[String] ⇒
          val valueName = args(0).asInstanceOf[String]
          checkNotNull(extractValue(valueName, method, dbo(valueName), method.getReturnType, mapping), valueName)
        case (_, method, _) ⇒ throw new IllegalAccessException("Cannot proxy methods with arguments: " + method)
      }
      fp.withEqualsHashCodeOverride(proxy)
    }

    private def isProxyable(cls: Class[_]): Boolean = cls.isInterface && cls.getMethods.forall { m =>
      m.getParameterTypes.length match {
        case 0 => true
        case 1 => m.getParameterTypes()(0) == classOf[String]
        case _ => false
      }
    }

  }

}
