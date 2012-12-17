package scuff

package object es {
  type Txn[ID, EVT] = EventSource[ID, EVT]#Transaction
}