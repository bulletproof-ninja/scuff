package scuff

import java.lang.management.ManagementFactory

import scala.reflect.{ ClassTag, classTag }
import scala.jdk.CollectionConverters._

import java.net.InetSocketAddress
import java.net.InetAddress
import java.util.concurrent.atomic.AtomicInteger

import javax.management._
import javax.management.remote.JMXServiceURL
import javax.management.remote.jmxmp.JMXMPConnectorServer
import java.beans.Introspector
import java.beans.PropertyDescriptor
import java.lang.reflect.Method
import java.util.concurrent.atomic.AtomicLong
import java.net.BindException
import scala.util.control.NonFatal

object JMX {

  final case class Registration(name: ObjectName)
  extends Subscription {
    require(Server isRegistered name)
    def cancel(): Unit = try {
      Server unregisterMBean name
    } catch {
      case NonFatal(cause) =>
        cause.printStackTrace(System.err)
    }
  }

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
    val name = mxBeanType.map(_.getSimpleName) || mxBean.getClass.getSimpleName
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

  private[this] final def MXBeanSuffix = "MXBean"
  private[this] final val Server: MBeanServer = ManagementFactory.getPlatformMBeanServer

  def startJMXMP(port: Int): JMXMPConnectorServer = startJMXMP(new InetSocketAddress(InetAddress.getLocalHost, port))
  def startJMXMP(address: InetSocketAddress = null): JMXMPConnectorServer = {
    val (jmxmpServer, urlOpt) = address match {
      case null =>
        val server = new JMXMPConnectorServer(Server)
        server -> Option(server.getAddress)
      case addr =>
        val url = new JMXServiceURL("jmxmp", addr.getAddress.getHostAddress, addr.getPort)
        new JMXMPConnectorServer(url, null, Server) -> Some(url)
    }

    try jmxmpServer.start() catch {
      case cause: BindException =>
        val target = urlOpt.map(url => s"${url.getHost}:${url.getPort}") || "default MBean server"
        val be = new BindException(s"Cannot bind to $target")
        be.setStackTrace(be.getStackTrace.take(1))
        be initCause cause
        throw be
    }
    jmxmpServer
  }

  def register(mxBean: AnyRef, instanceName: Option[String]): Registration =
    register(mxBean, instanceName, Map.empty[String, String])
  def register(mxBean: AnyRef, instanceName: Option[String], attributes: Map[String, String]): Registration = {
    val attrs = instanceName.foldLeft(attributes) {
      case (attrs, name) => attrs.updated("name", name)
    }
    register(mxBean, attrs)
  }
  def register(mxBean: AnyRef, instanceName: String = null, attributes: Map[String, String] = Map.empty): Registration =
    register(mxBean, instanceName.optional, attributes)

  def register(mxBean: AnyRef, attributes: Map[String, String]): Registration = {
    val objName = mkObjName(mxBean, attributes)
    register(mxBean, objName)
  }
  def register(mxBean: AnyRef, objectName: ObjectName): Registration = {
    Server.registerMBean(mxBean, objectName)
    Registration(objectName)
  }
  /**
    * Implement this trait if your bean have properties
    * that are unknown or variable at compile time.
    */
  trait DynamicMBean extends javax.management.DynamicMBean {

    /**
      * The dynamic properties.
      * NOTE: The keys must remain constant. Adding or removing keys
      * after startup will not be reflected, only changing values.
      */
    protected def dynamicProps: collection.Map[String, Any]
    protected def typeName = JMX.getTypeName(this, mxBeanType)

    private[this] val mxBeanType: Option[Class[_]] = mxBeanInterfaceOf(this)
    private[this] val (ops, props) = {
      mxBeanType match {
        case None =>
          Array.empty[(MBeanOperationInfo, Method)] -> Map.empty[String, PropertyDescriptor]
        case Some(interface) =>
          val props = Introspector.getBeanInfo(interface)
            .getPropertyDescriptors.map { prop =>
              prop.getName -> prop
            }.toMap
          val propMethods =
            props.values.map(_.getWriteMethod).filter(_ != null).toSet ++
              props.values.map(_.getReadMethod).filter(_ != null).toSet
          val ops = interface.getMethods.filterNot(propMethods).map { method =>
            new MBeanOperationInfo(s"Operation ${method.getName}", method) -> method
          }
          ops -> props
      }
    }

    def getAttribute(name: String) = getValue(name, dynamicProps)

    private def getValue(name: String, snapshot: => collection.Map[String, Any]): AnyRef = {
      props.get(name) match {
        case Some(prop) => prop.getReadMethod.invoke(this)
        case _ => snapshot(name).asInstanceOf[AnyRef]
      }
    }

    def setAttribute(attr: Attribute) = props(attr.getName).getWriteMethod.invoke(this, attr.getValue)
    def getAttributes(names: Array[String]) = {
      lazy val snapshot = dynamicProps
      val list = new AttributeList
      names.foreach { name =>
        val value = getValue(name, snapshot)
        list.add(new Attribute(name, value))
      }
      list
    }
    def setAttributes(list: AttributeList) = {
      list.asList.asScala.foreach(setAttribute)
      list
    }
    def invoke(name: String, values: Array[Object], types: Array[String]): Object = {
      val found = ops.find {
        case (_, method) =>
          method.getName == name && {
            val sig = method.getParameterTypes
            sig.length == values.length &&
              sig.zip(values).forall {
                case (argType, arg) => argType.isInstance(arg)
              }
          }
      }
      found.map(_._2.invoke(this, values: _*)).orNull
    }
    val getMBeanInfo = {
      val mapInfo = dynamicProps.map {
        case (name, value) =>
          name -> new MBeanAttributeInfo(name, value.getClass.getName, "", true, false, false)
      }.toMap
      val propInfo = props.values.map { prop =>
        val name = prop.getName
        name -> new MBeanAttributeInfo(name, s"$name description", prop.getReadMethod, prop.getWriteMethod)
      }.toMap
      val attrInfo = (mapInfo ++ propInfo).values.toArray
      val opsInfo = this.ops.map(_._1)
      new MBeanInfo(
        typeName, "",
        attrInfo,
        Array.empty[MBeanConstructorInfo],
        opsInfo,
        Array.empty[MBeanNotificationInfo])
    }
  }

  /**
    *  MBean implementations can extend this
    *  for easy sending of notifications.
    */
  abstract class NotificationMBean[N: ClassTag] extends NotificationBroadcasterSupport {
    private[this] val seqNums = new AtomicLong
    protected val notificationType: String = classTag[N].runtimeClass.getName
    protected def stringify(notif: N): String
    def sendNotification(notif: N, timestamp: Long = System.currentTimeMillis): Unit = {
      this sendNotification new Notification(
        notificationType,
        this,
        seqNums.getAndIncrement,
        timestamp,
        stringify(notif))
    }
  }

}
