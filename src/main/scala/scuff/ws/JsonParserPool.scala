package scuff.ws

import org.boon.json.JsonParserFactory

private object JsonParserFactoryPool extends scuff.ResourcePool(new JsonParserFactory)

private[ws] object JsonParserPool extends scuff.ResourcePool(JsonParserFactoryPool.borrow(_.createFastParser))
