package scuff.redis

import scala.concurrent.duration.FiniteDuration

class RedisCache(defaultTTL: FiniteDuration, conn: CONNECTION)
    extends BinaryRedisCache(defaultTTL, conn, StringSerializer, StringSerializer)
