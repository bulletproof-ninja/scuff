package scuff.web

import javax.activation.{ MimeType, MimeTypeParameterList }

final class AcceptHeader(mimeTypes: Seq[MimeType]) {
  def accepts(query: String): Boolean = accepts(new MimeType(query))
  def accepts(query: MimeType): Boolean = mimeTypes.exists { target ⇒
    query.getBaseType == target.getBaseType &&
      {
        val parms = query.getParameters
        parms.isEmpty || matches(parms, target.getParameters)
      }
  }
  private def matches(query: MimeTypeParameterList, target: MimeTypeParameterList): Boolean = {
    import collection.JavaConverters._
    query.getNames.asScala.asInstanceOf[Iterator[String]].forall { qn ⇒
      query.get(qn) == target.get(qn)
    }
  }
}

object AcceptHeader {
  private val Splitter = ",".r.pattern
  def apply(req: javax.servlet.http.HttpServletRequest): Option[AcceptHeader] = {
    Option(req.getHeader("Accept")).map { header ⇒
      val types = Splitter.split(header).map(new MimeType(_))
      new AcceptHeader(types)
    }
  }
}