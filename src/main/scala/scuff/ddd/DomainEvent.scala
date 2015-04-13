package scuff.ddd

import scuff.StreamingSerializer
import java.io.DataInput
import java.io.DataOutput
import java.io.OutputStream
import java.io.InputStream
import java.io.DataOutputStream
import java.io.DataInputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import scuff.JavaSerializer

/**
 * Domain event trait.
 */
trait DomainEvent extends scuff.Version with Serializable {
  def eventName = getClass.getSimpleName
}

/**
 * Type-class for generic domain event.
 */
trait DomainEvt[EVT] extends StreamingSerializer[EVT] {
  def version(evt: EVT): Short
  def name(evt: EVT): String
}

/**
 * Partial type-class instance for events extending [[scuff.ddd.DomainEvent]].
 */
trait DefaultDomainEvt[EVT <: DomainEvent] extends DomainEvt[EVT] {
  def version(evt: EVT): Short = evt.typeVersion
  def name(evt: EVT): String = evt.eventName
}

//trait DomainEventHandler[EVT <: DomainEvent] {
//  def apply(evt: EVT)
//}
