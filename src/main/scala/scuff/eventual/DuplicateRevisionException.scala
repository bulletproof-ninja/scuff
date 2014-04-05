package scuff.eventual

import scala.util.control.NoStackTrace

/**
 * There has been a concurrent revision update between
 * loading and updating the stream.
 */
class DuplicateRevisionException(id: Any, val revision: Int)
  extends RuntimeException("Revision %d already exists: %s".format(revision, id))
  with NoStackTrace
