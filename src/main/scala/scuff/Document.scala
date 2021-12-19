package scuff

import java.io.Writer

trait Document {
  /** Dump content to Writer. */
  def dump(out: java.io.Writer): Unit
  /** Mime type. */
  def mimeType: String
  /** Character encoding. */
  def encoding: String
}

object Document {
  val empty = new Document {
    def dump(out: Writer): Unit = ()
    def mimeType: String = "text/plain"
    def encoding: String = "UTF-8"
  }

  def apply(
      text: String,
      mediaType: MediaType = MediaType.`text/plain`)
      : Document =
    new Document {
      def dump(out: Writer): Unit = out write text
      def mimeType: String = mediaType.baseType
      def encoding: String = mediaType.parm("charset") || "UTF-8"
    }

}