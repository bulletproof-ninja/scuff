package scuff

import java.util.{ Date, UUID, Locale }
import com.mongodb._
import org.bson.types._
import scuff.js.CoffeeScriptCompiler
import scala.reflect.ClassTag

/**
 * A few MongoDB helper methods.
 */

object Mongolia {

  private lazy val coffeeCompiler = scuff.js.CoffeeScriptCompiler('bare -> true)

  final class Assignment(key: String) {
    def :=(value: BsonValue) = new BsonProp(key, value)
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
  final class BsonIntProp(key: String, intValue: Int) extends BsonProp(key, intValue) with BsonNumProp
  final class BsonLngProp(key: String, lngValue: Long) extends BsonProp(key, lngValue) with BsonNumProp
  final class BsonDblProp(key: String, dblValue: Double) extends BsonProp(key, dblValue) with BsonNumProp

  trait BsonValue { def raw: Any }
  implicit val SRgx2Val = (rgx: scala.util.matching.Regex) ⇒ Rgx2Val(rgx.pattern)
  implicit val Rgx2Val = (rgx: java.util.regex.Pattern) ⇒ new Value(rgx): BsonValue
  implicit val Str2Val = (str: String) ⇒ new Value(str): BsonValue
  implicit val Int2Val = (num: Int) ⇒ new Value(num): BsonValue
  implicit val Long2Val = (num: Long) ⇒ new Value(num): BsonValue
  implicit val Dbl2Val = (num: Double) ⇒ new Value(num): BsonValue
  implicit val Flt2Val = (num: Float) ⇒ new Value(num): BsonValue
  implicit val Shrt2Val = (num: Short) ⇒ new Value(num): BsonValue
  implicit val Bool2Val = (bool: Boolean) ⇒ new Value(bool): BsonValue
  implicit val Arr2Val = (array: Array[Byte]) ⇒ new Value(array): BsonValue
  implicit val Bin2Val = (binary: Binary) ⇒ new Value(binary): BsonValue
  implicit val Date2Val = (date: java.util.Date) ⇒ new Value(date): BsonValue
  implicit val OID2Val = (oid: ObjectId) ⇒ new Value(oid): BsonValue
  implicit val UUID2Val = (uuid: UUID) ⇒ {
    val bb = java.nio.ByteBuffer.allocate(16)
    bb.putLong(uuid.getMostSignificantBits).putLong(uuid.getLeastSignificantBits)
    new Binary(4, bb.array): BsonValue
  }
  implicit val Pwd2Val = (pwd: Password) ⇒ new Value(obj("digest" := pwd.digest, "salt" := pwd.salt, "algo" := pwd.algorithm)): BsonValue
  implicit val Eml2Val = (em: EmailAddress) ⇒ em.toString: BsonValue
  implicit val url2Val = (url: java.net.URL) ⇒ url.toString: BsonValue
  implicit val uri2Val = (uri: java.net.URI) ⇒ uri.toString: BsonValue
  implicit val tz2Val = (tz: java.util.TimeZone) ⇒ tz.getID: BsonValue
  implicit val gp2val = (gp: GeoPoint) ⇒ arr(gp.latitude: Double, gp.longitude: Double): BsonValue
  implicit val loc2Val = (locale: Locale) ⇒ locale.toLanguageTag: BsonValue

  implicit def Opt2Val[T <% BsonValue](opt: Option[T]): BsonValue = opt match {
    case None ⇒ null
    case Some(t) ⇒ (t: BsonValue)
  }
  implicit val Dbo2Val = (dbo: DBObject) ⇒ dbo match {
    case l: RichDBList ⇒ l: BsonValue
    case _ ⇒ enrich(dbo): BsonValue
  }

  implicit def Map2Val[T <% BsonValue](map: collection.Map[String, T]): BsonValue = {
    val dbo = new RichDBObject
    dbo.add(map)
    dbo
  }

  implicit def Arr2List[T <: AnyRef <% BsonValue](arr: Array[T]): RichDBList = Seq2List(arr)

  implicit def Seq2List[T <% BsonValue](seq: collection.GenTraversableOnce[T]): RichDBList = {
    val list = new RichDBList
    seq.foreach(t ⇒ list += (t: BsonValue).raw)
    list
  }

  implicit def id2obj(oid: ObjectId): DBObject = obj("_id" := oid)

  def obj(ignoreNulls: Boolean): RichDBObject = new RichDBObject(ignoreNulls = ignoreNulls)
  def obj(props: BsonProp*): RichDBObject = {
    val map = new RichDBObject
    props.foreach(p ⇒ if (map.put(p.key, p.raw) != null) throw new IllegalArgumentException("Field \"%s\" occurs multiple times".format(p.key)))
    map
  }
  def arr(values: BsonValue*): RichDBList = Seq2List(values)
  def obj[T <% BsonValue](map: collection.Map[String, T]): RichDBObject = new RichDBObject().add(map)

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

  implicit val Val2Byte = (value: BsonValue) ⇒ value.raw.asInstanceOf[Number].byteValue
  implicit val Val2Str = (value: BsonValue) ⇒ String.valueOf(value.raw)
  implicit val Val2Int = (value: BsonValue) ⇒ org.bson.BSON.toInt(value.raw)
  implicit val Val2Dbl = (value: BsonValue) ⇒ value.raw.asInstanceOf[Number].doubleValue
  implicit val Val2Flt = (value: BsonValue) ⇒ value.raw.asInstanceOf[Number].floatValue
  implicit val Val2Lng = (value: BsonValue) ⇒ value.raw match {
    case n: Number ⇒ n.longValue
    case d: Date ⇒ d.getTime
  }
  implicit val Val2Shrt = (value: BsonValue) ⇒ value.raw.asInstanceOf[Number].shortValue
  implicit val Val2Chr = (value: BsonValue) ⇒ value.raw match {
    case s: String if s.length == 1 ⇒ s.charAt(0)
    case i: Int if Char.MinValue <= i && i <= Char.MaxValue ⇒ i.asInstanceOf[Char]
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into Char".format(value.raw.getClass.getName))
  }
  implicit val Val2Bool = (value: BsonValue) ⇒ value.raw match {
    case b: java.lang.Boolean ⇒ b.booleanValue
    case _ ⇒ Val2Int.apply(value) != 0
  }
  def binaryType4ToUUID(array: Array[Byte]): UUID = {
    val bb = java.nio.ByteBuffer.wrap(array)
    new UUID(bb.getLong, bb.getLong)
  }
  implicit val Val2UUID = (value: BsonValue) ⇒ value.raw match {
    case u: UUID ⇒ u
    case b: Binary if b.getType == 4 ⇒ binaryType4ToUUID(b.getData)
    case a: Array[Byte] if a.length == 16 ⇒ binaryType4ToUUID(a)
    case s: String if s.length == 36 ⇒ UUID.fromString(s)
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into UUID".format(value.raw.getClass.getName))
  }
  implicit val Val2OID = (value: BsonValue) ⇒ ObjectId.massageToObjectId(value.raw)
  implicit val Val2Dec = (value: BsonValue) ⇒ value.raw match {
    case bd: java.math.BigDecimal ⇒ BigDecimal(bd)
    case d: Double ⇒ BigDecimal(d)
    case s: String ⇒ BigDecimal(s)
    case i: Int ⇒ BigDecimal(i)
    case l: Long ⇒ BigDecimal(l)
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into BigDecimal".format(value.raw.getClass.getName))
  }
  implicit val Val2Dbo = (value: BsonValue) ⇒ value.raw match {
    case list: BasicDBList ⇒ list
    case list: org.bson.LazyDBList ⇒ list
    case dbo: DBObject ⇒ dbo.enrich: DBObject
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into DBObject".format(value.raw.getClass.getName))
  }
  implicit val Val2Ts = (value: BsonValue) ⇒ value.raw match {
    case ts: Timestamp ⇒ ts
    case d: Date ⇒ new Timestamp(d)
    case l: Long ⇒ new Timestamp(l.longValue)
    case oid: ObjectId ⇒ new Timestamp(oid.getTime)
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into Timestamp".format(value.raw.getClass.getName))
  }
  implicit val Val2Bin = (value: BsonValue) ⇒ value.raw match {
    case a: Array[Byte] ⇒ a
    case b: Binary ⇒ b.getData
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into Array[Byte]".format(value.raw.getClass.getName))
  }
  implicit val Val2Locale = (value: BsonValue) ⇒ Locale.forLanguageTag(value.raw.asInstanceOf[String])
  implicit val Val2Gp = (value: BsonValue) ⇒ value.raw match {
    case list: java.util.List[_] if list.size == 2 ⇒
      val numList = list.asInstanceOf[java.util.List[Number]]
      new GeoPoint(latitude = numList.get(0).floatValue, longitude = numList.get(1).floatValue)
    case list: java.util.List[_] ⇒ throw new RuntimeException("Cannot coerce List.size != 2 into GeoPoint: %s".format(list))
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into GeoPoint".format(value.raw.getClass.getName))
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
  implicit val Val2Eml = (value: BsonValue) ⇒ new EmailAddress(value.raw.asInstanceOf[String])
  implicit val Val2Pwd = (value: BsonValue) ⇒ {
    val dbo = value.raw.asInstanceOf[DBObject]: RichDBObject
    new Password(dbo("digest").as[Array[Byte]], dbo("algo").as[String], dbo("salt").as[Array[Byte]])
  }
  implicit val Val2URL = (value: BsonValue) ⇒ new java.net.URL(String.valueOf(value.raw))
  implicit val Val2URI = (value: BsonValue) ⇒ new java.net.URI(String.valueOf(value.raw))
  implicit val val2Tz = (value: BsonValue) ⇒ {
    val tzID = String.valueOf(value.raw)
    val tz = java.util.TimeZone.getTimeZone(tzID)
    if (tz.getID == "GMT" && tzID != "GMT") throw new IllegalArgumentException("Unknown timezone: " + tzID)
    tz
  }
  implicit def Val2Map[T](implicit conv: BsonValue ⇒ T) = (value: BsonValue) ⇒ value.raw match {
    case dbo: DBObject ⇒ dbo.keys.map(key ⇒ key -> dbo(key).as[T]).toMap
    case _ ⇒ throw new RuntimeException("Cannot coerce %s into Map".format(value.raw.getClass.getName))
  }

  object BsonField {
    def apply(obj: Any, from: DBObject = null, key: String = null): BsonField = obj match {
      case null ⇒ if (from == null || from.containsField(key)) new Null(Option(key)) else new Missing(Option(key))
      case obj ⇒ new Value(obj)
    }
    def apply(obj: DBObject, key: String): BsonField = apply(obj.get(key), obj, key)
  }
  sealed trait BsonField {
    def opt[T](implicit conv: BsonValue ⇒ T): Option[T]
    def as[T](implicit conv: BsonValue ⇒ T): T
    def asSeq[T](implicit conv: BsonValue ⇒ T): IndexedSeq[T]
    def asSeqOfOption[T](implicit conv: BsonValue ⇒ T): IndexedSeq[Option[T]]
  }
  final class Null private[Mongolia] (fieldName: Option[String]) extends BsonField {
    def opt[T](implicit conv: BsonValue ⇒ T) = None
    def as[T](implicit conv: BsonValue ⇒ T): T = fieldName match {
      case None ⇒ throw new IllegalStateException("Field value is null")
      case Some(name) ⇒ throw new IllegalStateException("Field \"%s\" value is null".format(name))
    }
    def asSeqOfOption[T](implicit conv: BsonValue ⇒ T) = IndexedSeq.empty
    def asSeq[T](implicit conv: BsonValue ⇒ T) = IndexedSeq.empty
  }
  final class Missing private[Mongolia] (fieldName: Option[String]) extends BsonField {
    def opt[T](implicit conv: BsonValue ⇒ T) = None
    def as[T](implicit conv: BsonValue ⇒ T): T = fieldName match {
      case None ⇒ throw new IllegalStateException("Unknown field")
      case Some(name) ⇒ throw new IllegalStateException("Unknown field: \"%s\"".format(name))
    }
    def asSeqOfOption[T](implicit conv: BsonValue ⇒ T) = IndexedSeq.empty
    def asSeq[T](implicit conv: BsonValue ⇒ T) = IndexedSeq.empty
  }
  final class Value private[Mongolia] (val raw: Any) extends BsonField with BsonValue {
    def opt[T](implicit conv: BsonValue ⇒ T): Option[T] = Some(conv(this))
    def as[T](implicit conv: BsonValue ⇒ T): T = conv(this)
    def asSeqOfOption[T](implicit conv: BsonValue ⇒ T): IndexedSeq[Option[T]] = {
      val list: Iterable[_] = anyToIterable(raw)
      val array = new Array[Option[T]](list.size)
      var i = 0
      val iter = list.iterator
      while (i < array.length) {
        array(i) = iter.next match {
          case null ⇒ None
          case a ⇒ Some(conv(new Value(a)))
        }
        i += 1
      }
      array
    }
    def asSeq[T](implicit conv: BsonValue ⇒ T): IndexedSeq[T] = {
      val list: Iterable[_] = anyToIterable(raw)
      val array = new Array[Any](list.size)
      var i = 0
      val iter = list.iterator
      while (i < array.length) {
        iter.next match {
          case null ⇒ // Ignore
          case a ⇒ array(i) = conv(new Value(a))
        }
        i += 1
      }
      array.toIndexedSeq.asInstanceOf[IndexedSeq[T]]
    }
  }

  final class RichDB(underlying: DB) {
    def impoverish = underlying
    def apply(collection: String) = underlying.getCollection(collection): RichDBCollection
    def enrich: RichDB = this
  }

  final class RichDBCollection(underlying: DBCollection) {
    def impoverish = underlying
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
    def upsert(key: DBObject, upd: DBObject) = underlying.update(key, upd, true, false)
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

    def ensureIndex(key: String): Unit = ensureIndex(key := ASC)
    def ensureIndex(key: String, idxType: String): Unit = underlying.ensureIndex(obj(key := idxType))
    def ensureIndex(keys: BsonIntProp*): Unit = underlying.ensureIndex(obj(keys: _*))
    def ensureUniqueIndex(key: String): Unit = ensureUniqueIndex(key := ASC)
    def ensureSparseIndex(key: String): Unit = underlying.ensureIndex(obj(key := ASC), obj("sparse" := true))
    def ensureUniqueSparseIndex(key: String): Unit = underlying.ensureIndex(obj(key := ASC), obj("sparse" := true, "unique" := true))
    def ensureUniqueIndex(keys: BsonIntProp*): Unit = underlying.ensureIndex(obj(keys: _*), obj("unique" := true))
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
   * and more importantly *correct* JSON.
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

  final class RichDBObject(private val underlying: DBObject = new BasicDBObject, ignoreNulls: Boolean = false) extends DBObject with BsonValue {
    import collection.JavaConverters._
    if (underlying == null) throw new NullPointerException("Document is null")
    def markAsPartialObject = underlying.markAsPartialObject
    def isPartialObject: Boolean = underlying.isPartialObject
    def put(key: String, v: Any) = if (ignoreNulls && v == null) underlying.removeField(key) else underlying.put(key, v)
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
    def add[T <% BsonValue](map: collection.Map[String, T]): RichDBObject = {
      map.foreach {
        case (key, value) ⇒ add(key := value)
      }
      this
    }
    def rename(fromTo: (String, String), transform: BsonField ⇒ BsonValue = null): Unit = {
      val removed = underlying.removeField(fromTo._1)
      if (transform == null) {
        underlying.put(fromTo._2, removed)
      } else {
        val value = transform(BsonField(removed, null, fromTo._1))
        this.add(fromTo._2 := value)
      }
    }
    def raw = this
    def ignoreNulls(ignore: Boolean): RichDBObject = if (ignore == this.ignoreNulls) this else new RichDBObject(underlying, ignore)
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

    def as[T](converters: Map[Class[_], BsonValue ⇒ Any] = Map.empty)(implicit manifest: ClassTag[T]): T =
      if (manifest.runtimeClass.isInterface) getProxy(this, converters) else throw new IllegalArgumentException("%s must be an interface".format(manifest.runtimeClass))

    def isEmpty = underlying match {
      case m: java.util.Map[_, _] ⇒ m.isEmpty
      case _ ⇒ underlying.keySet.isEmpty
    }
    def prop(key: String): BsonProp = new BsonProp(key, new Value(underlying.get(key)))
    def asSeq[T](implicit conv: BsonValue ⇒ T) = new Value(underlying).asSeq[T]
    def asSeqOfOption[T](implicit conv: BsonValue ⇒ T) = new Value(underlying).asSeqOfOption[T]
    def +=(prop: BsonProp): Unit = this.put(prop.key, prop.raw)
    def -=(key: String): Unit = this.removeField(key)
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
     * and more importantly *correct* JSON.
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
    def first(sorting: BsonIntProp*): Option[RichDBObject] = cursor.sort(obj(sorting: _*)).limit(1).nextOpt()
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
    def flatMap[T](f: RichDBObject ⇒ Option[T]): Seq[T] = {
      val buffer = collection.mutable.Buffer[T]()
      foreach { dbo ⇒
        f(dbo) match {
          case Some(t) ⇒ buffer += t
          case None ⇒ // Filter out
        }
      }
      buffer
    }
  }

  implicit def prop2obj(prop: BsonProp) = obj(prop)

  private def iter2List[T <% BsonValue](i: java.lang.Iterable[T]) = {
    val list = new RichDBList
    var iter = i.iterator()
    while (iter.hasNext) {
      val t: T = iter.next
      list += t.raw
    }
    list
  }

  def _id(value: BsonValue) = "_id" := value
  def $gt(value: BsonValue) = "$gt" := value
  def $gte(value: BsonValue) = "$gte" := value
  def $lt(value: BsonValue) = "$lt" := value
  def $ne(value: BsonValue) = "$ne" := value
  def $lte(value: BsonValue) = "$lte" := value
  def $size(size: Int) = "$size" := size
  def $type(bsonType: BsonType) = "$type" := bsonType.typeNumber
  def $all[T <% BsonValue](values: T*) = "$all" := arr(values.map(t ⇒ t: BsonValue): _*)
  def $in[T <% BsonValue](values: T*) = "$in" := arr(values.map(t ⇒ t: BsonValue): _*)
  def $nin[T <% BsonValue](values: T*) = "$nin" := arr(values.map(t ⇒ t: BsonValue): _*)
  def $and[T <% BsonValue](exprs: T*) = "$and" := arr(exprs.map(t ⇒ t: BsonValue): _*)
  def $or[T <% BsonValue](exprs: T*) = "$or" := arr(exprs.map(t ⇒ t: BsonValue): _*)
  def $nor[T <% BsonValue](exprs: T*) = "$nor" := arr(exprs.map(t ⇒ t: BsonValue): _*)
  def $each[T <% BsonValue](values: T*) = "$each" := arr(values.map(t ⇒ t: BsonValue): _*)
  def $exists(exists: Boolean) = "$exists" := exists
  def $set(props: BsonProp*) = "$set" := obj(props: _*)
  def $unset(names: String*) = {
    val unsets = new RichDBObject
    names.foreach { name ⇒
      unsets.add(name := 1)
    }
    "$unset" := unsets
  }
  def $mod(modBy: Int, equalsTo: Int) = "$mod" := arr(modBy, equalsTo)
  def $not(props: BsonProp*) = "$not" := obj(props: _*)
  def $inc(props: BsonNumProp*) = "$inc" := obj(props: _*)
  def $push(props: BsonProp*) = "$push" := obj(props: _*)
  def $addToSet(prop: BsonProp) = "$addToSet" := obj(prop)
  def $pushAll(prop: BsonProp) = "$pushAll" := obj(prop)
  def $pop(prop: BsonIntProp): BsonProp = "$pop" := obj(prop)
  def $pop(name: String): BsonProp = $pop(name := LAST)
  def $pull(prop: BsonProp) = "$pull" := obj(prop)
  def $elemMatch(props: BsonProp*) = "$elemMatch" := obj(props: _*)
  def $near(point: GeoPoint): BsonProp = "$near" := point
  def $near(point: GeoPoint, maxDistance: Int): RichDBObject = obj($near(point), $maxDistance(maxDistance))
  def $maxDistance(meters: Int) = "$maxDistance" := meters
  def $regex(regex: String, options: String = "") = {
    val dbo = obj("$regex" := regex)
    if (options.length != 0) dbo.add("$options" := options)
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
      require(map.size > 0, "`map` function not found. Must be named `map`.")
      require(reduce.size > 0, "`reduce` function not found. Must be named `reduce`.")
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

  private val DefaultProxyConverters: Map[Class[_], (BsonValue ⇒ Any)] = Map(
    classOf[String] -> Val2Str,
    classOf[Double] -> Val2Dbl,
    classOf[Float] -> Val2Dbl,
    classOf[Long] -> Val2Lng,
    classOf[Int] -> Val2Int,
    classOf[Byte] -> Val2Byte,
    classOf[Boolean] -> Val2Bool,
    classOf[Short] -> Val2Shrt,
    classOf[Char] -> Val2Chr,
    classOf[UUID] -> Val2UUID,
    classOf[BigDecimal] -> Val2Dec,
    classOf[ObjectId] -> Val2OID,
    classOf[Array[Byte]] -> Val2Bin,
    classOf[scuff.Timestamp] -> Val2Ts,
    classOf[java.util.Date] -> Val2Ts,
    classOf[java.util.TimeZone] -> val2Tz,
    classOf[java.util.Locale] -> Val2Locale,
    classOf[Number] -> Val2Dec,
    classOf[scuff.EmailAddress] -> Val2Eml,
    classOf[scuff.Password] -> Val2Pwd,
    classOf[java.net.URL] -> Val2URL,
    classOf[java.net.URI] -> Val2URI)

  private def convertProxyValue(value: BsonField, asType: Class[_], converters: Map[Class[_], BsonValue ⇒ Any]) = converters.get(asType) match {
    case Some(converter) ⇒ value.as(converter)
    case None ⇒
      if (asType.isInterface) {
        getProxy(value.as[DBObject], converters)(ClassTag(asType))
      } else {
        value match {
          case value: Value ⇒ value.raw
          case _ ⇒ null
        }
      }
  }
  private def convertProxyOption(value: BsonField, optType: Class[_], converters: Map[Class[_], BsonValue ⇒ Any]) = converters.get(optType) match {
    case Some(converter) ⇒ value.opt(converter)
    case None ⇒
      if (optType.isInterface) {
        value.opt[DBObject].map(getProxy(_, converters)(ClassTag(optType)))
      } else {
        value match {
          case value: Value ⇒ Option(convertProxyValue(value, optType, converters))
          case _ ⇒ None
        }
      }
  }
  private def convertProxySeq(shouldBeList: BsonField, seqType: Class[_], converters: Map[Class[_], BsonValue ⇒ Any]) = converters.get(seqType) match {
    case Some(converter) ⇒ shouldBeList.asSeq(converter)
    case None ⇒
      if (seqType.isInterface) {
        shouldBeList.asSeq[DBObject].map(getProxy(_, converters)(ClassTag(seqType)))
      } else {
        shouldBeList match {
          case value: Value ⇒ value.raw match {
            case list: java.util.ArrayList[_] ⇒
              import collection.JavaConverters._
              list.asScala.map(elem ⇒ convertProxyValue(BsonField(elem), seqType, converters))
            case _ ⇒ Seq(convertProxyValue(value, seqType, converters))
          }
          case _ ⇒ Seq.empty
        }
      }
  }

  private def getGenericReturnClass(method: java.lang.reflect.Method) =
    method.getGenericReturnType().asInstanceOf[java.lang.reflect.ParameterizedType].getActualTypeArguments().head.asInstanceOf[Class[_]]

  private def getProxy[T: ClassTag](dbo: RichDBObject, userConverters: Map[Class[_], BsonValue ⇒ Any]): T = {
    val fp = new Proxylicious[T]
    val converters = DefaultProxyConverters ++ userConverters
    fp.proxify {
      case (_, method, args) if args == null || args.length == 0 ⇒
        if (method.getName == "toString") {
          dbo.toJson()
        } else {
          val value = dbo(method.getName)
          val rt = method.getReturnType
          if (rt == classOf[Option[_]]) {
            val rtt = getGenericReturnClass(method)
            convertProxyOption(value, rtt, converters)
          } else if (rt.isAssignableFrom(classOf[Seq[_]])) {
            val rtt = getGenericReturnClass(method)
            convertProxySeq(value, rtt, converters)
          } else {
            convertProxyValue(value, rt, converters)
          }
        }
      case (_, method, _) ⇒ throw new IllegalAccessException("Cannot proxy methods with arguments: " + method)
    }
  }

}
