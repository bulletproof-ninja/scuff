package scuff.eventual.ddd.cqrs

import scuff.Faucet

trait DataStore {

  type CONN

  def wipeAll()

  def connect[T](func: CONN => T): T

}
