package scuff

import java.io.{ BufferedWriter, ByteArrayOutputStream, File, FileInputStream, OutputStream, OutputStreamWriter }
import java.net.{ InetAddress, InetSocketAddress }

import language.implicitConversions

import javax.activation.DataHandler
import javax.mail.{ Address, Message, MessagingException, PasswordAuthentication, Session, Transport }
import javax.mail.internet.{ InternetAddress, MimeBodyPart, MimeMessage, MimeMultipart }
import javax.mail.util.ByteArrayDataSource

trait MailRoom {
  import MailRoom.Attachment

  /**
   * Send email.
   * @param from The sender
   * @param subject The email header
   * @param body The email body
   * @param toAll The recipient(s)
   * @throws MessagingException If email could not be sent
   */
  @throws(classOf[MessagingException])
  def send(from: InternetAddress, subject: String, body: Document, toAll: Iterable[InternetAddress], attachments: Attachment*)

  /**
   * Send email.
   * @param from The sender
   * @param subject The email header
   * @param body The email body
   * @param to The recipient
   * @param toMore More recipients
   * @throws MessagingException If email could not be sent
   */
  @throws(classOf[MessagingException])
  final def send(from: InternetAddress, subject: String, body: Document, to: InternetAddress, toMore: InternetAddress*) {
    send(from, subject, body, Seq(to) ++ toMore)
  }

}

private class JavaxMailRoom(session: Session, headers: (String, String)*) extends MailRoom {

  import MailRoom.Attachment

  private def toDataHandler(doc: Document) = {
    val out = new ByteArrayOutputStream
    val wrt = new BufferedWriter(new OutputStreamWriter(out, doc.encoding))
    doc.dump(wrt)
    wrt.close()
    val contentType = new javax.mail.internet.ContentType(doc.mimeType)
    contentType.getParameter("charset") match {
      case null ⇒ contentType.setParameter("charset", doc.encoding)
      case cs ⇒ require(cs.toUpperCase == doc.encoding.toUpperCase, "MIME-type charset and encoding do not match: %s vs. %s".format(cs, doc.encoding))
    }
    new DataHandler(new ByteArrayDataSource(out.toByteArray, contentType.toString))
  }
  private def toDataHandler(doc: Attachment) = {
    val out = new ByteArrayOutputStream
    doc.dump(out)
    out.close()
    val contentType = new javax.mail.internet.ContentType(doc.mimeType)
    new DataHandler(new ByteArrayDataSource(out.toByteArray, contentType.toString))
  }

  def send(from: InternetAddress, subject: String, body: Document, toAll: Iterable[InternetAddress], attachments: Attachment*) {
    val msg = new MimeMessage(session)
    msg.setFrom(from)
    msg.setRecipients(Message.RecipientType.TO, toAll.toArray[Address])
    msg.setSubject(subject)
    val multipart = new MimeMultipart
    val bodyPart = new MimeBodyPart
    bodyPart.setDataHandler(toDataHandler(body))
    multipart.addBodyPart(bodyPart)
    attachments.foreach { attachment ⇒
      val part = new MimeBodyPart
      part.setFileName(attachment.name)
      part.setDataHandler(toDataHandler(attachment))
      multipart.addBodyPart(part)
    }
    msg.setContent(multipart)
    headers.foreach {
      case (key, value) ⇒ msg.setHeader(key, value)
    }
    Transport.send(msg)
  }

}

object MailRoom {

  object Implicits {
    implicit def toInternetAddress(email: EmailAddress) = new InternetAddress(email.toString, true)
    implicit def toInternetAddress(email: String) = new InternetAddress(email, true)
    implicit def toSocketAddress(addr: InetAddress) = new InetSocketAddress(addr, DefaultSmtpPort)
  }

  private final val DefaultSmtpPort = 25

  def apply(smtpServer: InetSocketAddress, username: String, password: String, headers: (String, String)*): MailRoom =
    new JavaxMailRoom(makeSession(smtpServer, Some((username, password))), headers: _*)

  def apply(smtpServer: InetSocketAddress, headers: (String, String)*): MailRoom =
    new JavaxMailRoom(makeSession(smtpServer, None), headers: _*)

  def makeSession(smtpServer: InetSocketAddress, userPass: Option[(String, String)], otherProps: Map[String, Any] = Map.empty) = {
    val props = new java.util.Properties
    props.setProperty("mail.smtp.host", smtpServer.getHostName)
    props.setProperty("mail.smtp.port", smtpServer.getPort.toString)
    val auth = userPass.map {
      case (user, pass) ⇒
        props.setProperty("mail.smtp.auth", "true")
        props.setProperty("mail.smtp.submitter", user)
        new javax.mail.Authenticator {
          override def getPasswordAuthentication = new PasswordAuthentication(user, pass)
        }
    }.getOrElse(null)
    otherProps.foreach {
      case (key, value) ⇒ props.setProperty(key, String.valueOf(value))
    }
    Session.getInstance(props, auth)
  }

  trait Attachment {
    def name: String
    def dump(out: OutputStream)
    def mimeType: String
  }
  object Attachment {
    def apply(doc: Document, name: String): Attachment = new DocAttachment(doc, name)
    def apply(file: File, mimeType: String, altName: String = null): Attachment = new FileAttachment(file, mimeType, Option(altName))
  }
  private class DocAttachment(doc: Document, val name: String) extends Attachment {
    require(name != null, "Must have document name")
    def dump(out: OutputStream) {
      val writer = new OutputStreamWriter(out)
      doc.dump(writer)
      writer.close()
    }
    def mimeType: String = doc.mimeType
  }
  private class FileAttachment(file: File, val mimeType: String, altName: Option[String]) extends Attachment {
    val name = altName.getOrElse(file.getName)
    def dump(out: OutputStream) {
      val is = new FileInputStream(file)
      try {
        IO.copyStream(is -> out)
      } finally {
        is.close()
      }
    }
  }

}
