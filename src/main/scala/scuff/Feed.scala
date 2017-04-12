package scuff

trait Feed {
  type Selector
  type Consumer

  /** Start subscription. */
  def subscribe(include: Selector => Boolean = Function.const(true))(s: Consumer): Subscription
}
