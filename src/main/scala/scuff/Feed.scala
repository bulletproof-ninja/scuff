package scuff

trait Feed {
  type Selector
  type Consumer

  /** Start subscription. */
  def subscribe(include: Selector => Boolean = _ => true)(s: Consumer): Subscription
}
