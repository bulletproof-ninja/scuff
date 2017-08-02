package scuff

import java.lang.management.ManagementFactory

import scala.reflect.{ ClassTag, classTag }
import scala.collection.JavaConverters._

import java.net.InetSocketAddress
import java.net.InetAddress
import java.util.concurrent.atomic.AtomicInteger

import javax.management._
import javax.management.remote.JMXServiceURL
import javax.management.remote.jmxmp.JMXMPConnectorServer

object JMX {

  private[this] val unsafeChars = Array(' ', '*', '?', '=', ':', '"', '\n', '\\', '/', ',')
  private[this] val nameCounters = new Memoizer[String, AtomicInteger](_ => new AtomicInteger)
  private def mxBeanInterfaceOf(mxBean: AnyRef): Option[Class[_]] = {
    val MXBeanAnnotationClass = classOf[MXBean]
    mxBean.getClass.getInterfaces.find { i =>
      i.getName.endsWith(MXBeanSuffix) ||
        i.getAnnotations.map(_.annotationType).exists {
          case MXBeanAnnotationClass => true
          case _ => false
        }
    }
  }
  private def getTypeName(mxBean: AnyRef, mxBeanType: Option[Class[_]]): String = {
    val name = mxBeanType.map(_.getSimpleName) getOrElse mxBean.getClass.getSimpleName
    if (name.length > MXBeanSuffix.length && name.endsWith(MXBeanSuffix)) {
      name.substring(0, name.length - MXBeanSuffix.length)
    } else name
  }
  private def mkObjName(mxBean: AnyRef, attrs: Map[String, String]): ObjectName = {
      def isQuoted(name: String) = name.startsWith("\"") && name.endsWith("\"")
      def needsQuotes(name: String) = !isQuoted(name) && unsafeChars.exists(name.indexOf(_) != -1)
      def safeName(name: String): String = {
        nameCounters(name).getAndIncrement match {
          case 0 =>
            if (needsQuotes(name)) {
              ObjectName quote name
            } else name
          case n =>
            if (isQuoted(name)) {
              s""""${name.substring(1, name.length - 1)}[$n]""""
            } else if (needsQuotes(name)) {
              ObjectName quote s"$name[$n]"
            } else s"$name[$n]"
        }
      }
    val mxBeanInterface = mxBeanInterfaceOf(mxBean)
    val attributes = Map("type" -> getTypeName(mxBean, mxBeanInterface)) ++ attrs.map {
      case ("name", name) => "name" -> safeName(name)
      case entry => entry
    }
    new ObjectName(mxBean.getClass.getPackage.getName, new java.util.Hashtable(attributes.asJava))
  }

  /**
    *  MBean implementation classes can extend this
    *  class for easy sending of notifications.
    */
  abstract class Notifications[N: ClassTag] extends NotificationBroadcasterSupport {
    protected val notificationType: String = classTag[N].runtimeClass.getName
    protected def stringify(notif: N): String
    def sendNotification(notif: N, seqNum: Long, timestamp: Long = System.currentTimeMillis): Unit = {
      this sendNotification new Notification(
        notificationType,
        this,
        seqNum,
        timestamp,
        stringify(notif))
    }
  }

  private[this] final val MXBeanSuffix = "MXBean"
  private[this] final val Server: MBeanServer = ManagementFactory.getPlatformMBeanServer

  def startJMXMP(port: Int): JMXServiceURL = startJMXMP(new InetSocketAddress(InetAddress.getLocalHost, port))
  def startJMXMP(address: InetSocketAddress = null): JMXServiceURL = {
    val jmxmpServer = address match {
      case null =>
        new JMXMPConnectorServer(Server)
      case addr =>
        val url = new JMXServiceURL("jmxmp", addr.getAddress.getHostAddress, addr.getPort)
        new JMXMPConnectorServer(url, null, Server)
    }
    jmxmpServer.start()
    jmxmpServer.getAddress
  }

  def register(mxBean: AnyRef, instanceName: Option[String]): ObjectName =
    register(mxBean, instanceName, Map.empty[String, String])
  def register(mxBean: AnyRef, instanceName: Option[String], attributes: Map[String, String]): ObjectName = {
    val attrs = instanceName.foldLeft(attributes) {
      case (attrs, name) => attrs.updated("name", name)
    }
    register(mxBean, attrs)
  }
  def register(mxBean: AnyRef, instanceName: String = null, attributes: Map[String, String] = Map.empty): ObjectName =
    register(mxBean, instanceName.optional, attributes)

  def register(mxBean: AnyRef, attributes: Map[String, String]): ObjectName = {
    val objName = mkObjName(mxBean, attributes)
    register(mxBean, objName)
    objName
  }
  def register(mxBean: AnyRef, objectName: ObjectName): Unit =
    Server.registerMBean(mxBean, objectName)

}
