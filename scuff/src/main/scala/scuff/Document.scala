package scuff

trait Document {
  def dump(out: java.io.Writer)
  def mimeType: String
  def encoding: String
}