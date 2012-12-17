package scuff.ddd.util

import scuff.ddd._

trait EventDataSerializer[EVT <: DomainEvent, S] {
  def snapshotData(evt: EVT): S
  def rebuildEvent(version: Short, data: S): EVT
}
