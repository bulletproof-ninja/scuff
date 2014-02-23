package scuff.redis

class RedisCache(defaultTTL: Int, conn: CONNECTION)
    extends BinaryRedisCache(defaultTTL, conn, StringSerializer, StringSerializer)
