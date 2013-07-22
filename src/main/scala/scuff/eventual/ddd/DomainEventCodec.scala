package scuff.eventual.ddd

import scuff.ddd._

trait DomainEventCodec[EVT <: DomainEvent, S] {
  def encodeEvent(evt: EVT): S
  def decodeEvent(version: Short, data: S): EVT
}
