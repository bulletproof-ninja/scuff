package scuff

trait Subscription {
  def cancel()
}

trait Channel {
  type L
  def subscribe(s: L): Subscription
}