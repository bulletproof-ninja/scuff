package scuff

import org.boon.json.JsonParserFactory

private object JsonParserFactoryPool
  extends concurrent.ResourcePool(new JsonParserFactory)

private[scuff] object JsonParserPool
  extends concurrent.ResourcePool(JsonParserFactoryPool.use(_.createFastParser))
