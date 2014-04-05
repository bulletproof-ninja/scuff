package scuff

import java.io.{BufferedWriter, ByteArrayOutputStream, File, FileInputStream, OutputStream, OutputStreamWriter}
import java.net.{InetAddress, InetSocketAddress}

import language.implicitConversions

import javax.activation.DataHandler
import javax.mail.{Address, Message, MessagingException, PasswordAuthentication, Session, Transport}
import javax.mail.internet.{InternetAddress, MimeBodyPart, MimeMessage, MimeMultipart}
import javax.mail.util.ByteArrayDataSource

class MailRoom(session: Session, headers: (String, String)*) {
  def this(smtpServer: InetSocketAddress, username: String, password: String, headers: (String, String)*) = this(MailRoom.makeSession(smtpServer, Some((username, password))), headers: _*)
  def this(smtpServer: InetSocketAddress, headers: (String, String)*) = this(MailRoom.makeSession(smtpServer, None), headers: _*)

  import MailRoom._

  /**
   * Send email.
   * @param from The sender
   * @param subject The email header
   * @param body The email body
    * @param toAll The recipient(s)
   * @throws MessagingException If email could not be sent
   */
  @throws(classOf[MessagingException])
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
  def send(from: InternetAddress, subject: String, body: Document, to: InternetAddress, toMore: InternetAddress*) {
    send(from, subject, body, Seq(to) ++ toMore)
  }

}

object MailRoom {

  implicit def toInternetAddress(email: EmailAddress) = new InternetAddress(email.toString, true)
  implicit def toInternetAddress(email: String) = new InternetAddress(email, true)
  implicit def toSocketAddress(addr: InetAddress) = new InetSocketAddress(addr, DefaultSmtpPort)
  implicit def toSocketAddress(addr: String) = new InetSocketAddress(addr, DefaultSmtpPort)

  private final val DefaultSmtpPort = 25

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
  class DocAttachment(doc: Document, val name: String) extends Attachment {
    require(name != null, "Must have document name")
    def dump(out: OutputStream) {
      val writer = new OutputStreamWriter(out)
      doc.dump(writer)
      writer.close()
    }
    def mimeType: String = doc.mimeType
  }
  class FileAttachment(file: File, val mimeType: String, altName: String = null) extends Attachment {
    val name = Option(altName).getOrElse(file.getName)
    def dump(out: OutputStream) {
      val is = new FileInputStream(file)
      IO.copyStream(is -> out)
    }
  }

}
