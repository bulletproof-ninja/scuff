package scuff

import java.io._

import javax.activation.DataHandler

import java.net.{ InetSocketAddress, InetAddress }

import javax.mail._, internet.{ MimeMessage, InternetAddress }
import javax.mail.util.ByteArrayDataSource

class MailRoom(session: Session, headers: (String, String)*) {
  def this(smtpServer: InetSocketAddress, username: String, password: String, headers: (String, String)*) = this(MailRoom.makeSession(smtpServer, Some(username, password)), headers: _*)
  def this(smtpServer: InetSocketAddress, headers: (String, String)*) = this(MailRoom.makeSession(smtpServer, None), headers: _*)

  /**
   * Send email.
   * @param from The sender
   * @param subject The email header
   * @param body The email body
    * @param toAll The recipient(s)
   * @throws MessagingException If email could not be sent
   */
  @throws(classOf[MessagingException])
  def send(from: InternetAddress, subject: String, body: Document, toAll: Iterable[InternetAddress]) {
    val contentType = new javax.mail.internet.ContentType(body.mimeType)
    contentType.getParameter("charset") match {
      case null ⇒ contentType.setParameter("charset", body.encoding)
      case cs ⇒ require(cs.toUpperCase == body.encoding.toUpperCase, "MIME-type charset and encoding do not match: %s vs. %s".format(cs, body.encoding))
    }
    val out = new ByteArrayOutputStream
    val wrt = new BufferedWriter(new OutputStreamWriter(out, body.encoding))
    body.dump(wrt)
    wrt.close()
    val msg = new MimeMessage(session)
    msg.setFrom(from)
    msg.setRecipients(Message.RecipientType.TO, toAll.toArray[Address])
    msg.setSubject(subject)
    msg.setDataHandler(new DataHandler(new ByteArrayDataSource(out.toByteArray, contentType.toString)))
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

  private def makeSession(smtpServer: InetSocketAddress, userPass: Option[(String, String)]) = {
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
    Session.getInstance(props, auth)
  }

}
