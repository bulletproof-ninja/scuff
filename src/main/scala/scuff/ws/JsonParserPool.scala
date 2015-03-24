package scuff.ws

import org.boon.json.JsonParserFactory
import scuff.concurrent.ResourcePool

private object JsonParserFactoryPool extends ResourcePool(new JsonParserFactory)

private[ws] object JsonParserPool extends ResourcePool(JsonParserFactoryPool.borrow(_.createFastParser))
