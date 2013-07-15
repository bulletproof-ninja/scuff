package scuff.eventual

/**
 * There has been a concurrent revision update between
 * loading and updating the stream.
 */
class DuplicateRevisionException(id: Any, revision: Long) extends RuntimeException("Revision %d already exists: %s".format(revision, id))
