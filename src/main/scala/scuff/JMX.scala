package scuff

import java.lang.management.ManagementFactory

import scala.reflect.{ ClassTag, classTag }

import javax.management._
import javax.management.remote.JMXServiceURL
import javax.management.remote.jmxmp.JMXMPConnectorServer
import java.net.InetSocketAddress
import java.net.InetAddress
import java.util.concurrent.atomic.AtomicInteger

object JMX {

  private[this] val nameCounters = new Memoizer[String, AtomicInteger](_ => new AtomicInteger)
  private def uniqueObjectName(name: String): String =
    nameCounters(name).getAndIncrement match {
      case 0 => ObjectName quote name
      case n => ObjectName quote s"$name [$n]"
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

  private[this] final val Suffix = "MXBean"
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

  private def mkObjName(mxBean: AnyRef, name: Option[String]): ObjectName = {
    val MXBeanAnnotationClass = classOf[MXBean]
    val nameAttr = name.map(name => s",name=${uniqueObjectName(name)}") getOrElse ""
    val mxBeanInterface = mxBean.getClass.getInterfaces.find { i =>
      i.getName.endsWith(Suffix) ||
        i.getAnnotations.map(_.annotationType).exists {
          case MXBeanAnnotationClass => true
          case _ => false
        }
    }
    val typeName = (mxBeanInterface.map(_.getSimpleName) getOrElse mxBean.getClass.getSimpleName) match {
      case name if name.length > Suffix.length && name.endsWith(Suffix) => name.substring(0, name.length - Suffix.length)
      case name => name
    }
    new ObjectName(s"${mxBean.getClass.getPackage.getName}:type=$typeName$nameAttr")
  }

  def register(mxBean: AnyRef, instanceName: Option[String]): ObjectName = {
    val objName = mkObjName(mxBean, instanceName)
    register(mxBean, objName)
    objName
  }

  def register(mxBean: AnyRef, instanceName: String = null): ObjectName = register(mxBean, Option(instanceName))

  def register(mxBean: AnyRef, objectName: ObjectName): Unit = {
    Server.registerMBean(mxBean, objectName)
  }

}
