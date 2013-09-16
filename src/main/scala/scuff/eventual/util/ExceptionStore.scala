package scuff.eventual.util

import com.mongodb.DB
import scuff.Mongolia._
import com.mongodb.DBObject
import collection.JavaConverters._
import com.mongodb.{ DB, DBObject }
import redis.clients.jedis.{ JedisPool, JedisShardInfo }
import scuff.Threads.PiggyBack
import scuff.Mongolia.{ enrich, impoverish }
import scuff.redis.{ RedisChannel, RedisConnectionPool, RedisPublisher, threadSafe }
import com.mongodb.WriteConcern
import scuff.Mongolia._

trait ExceptionStore extends scuff.eventual.EventStore[String, List[StackTraceElement], Class[_]] {

  def store(e: Exception, metadata: Map[String, String] = Map.empty) {
      def getStackTraces(t: Throwable, stackTraces: List[List[StackTraceElement]] = Nil): List[List[StackTraceElement]] = {
        if (t != null) {
          getStackTraces(t.getCause, t.getStackTrace.toList :: stackTraces)
        } else {
          stackTraces.reverse
        }
      }
    append(e.getClass, e.toString, getStackTraces(e), metadata)
  }

}
