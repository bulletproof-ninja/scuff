package scuff

trait Document {
  def dump(out: java.io.Writer): Unit
  def mimeType: String
  def encoding: String
}
