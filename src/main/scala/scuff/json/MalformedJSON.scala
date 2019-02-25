package scuff.json

class MalformedJSON(msg: String, cause: Throwable = null)
  extends IllegalArgumentException(msg, cause)
