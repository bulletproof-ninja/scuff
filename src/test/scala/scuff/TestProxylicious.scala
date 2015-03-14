package scuff

import org.junit._
import org.junit.Assert._

import java.lang.reflect.Method

import scala.util._

class TestProxylicious {

  @Test def `general test` {
    val multiply = new Arithmetic {
      def apply(a: Int, b: Int) = a * b
    }
    val proxyfier = new Proxylicious[Arithmetic]
    val doubler = new proxyfier.Sandwich {
      def include(method: Method) = {
        method.getParameterTypes match {
          case Array(Integer.TYPE, Integer.TYPE) => method named 'apply returns Integer.TYPE
          case _ => false
        }
      }
      def before(proxy: Arithmetic, method: Method, args: Array[Any]) {}
      def after(proxy: Arithmetic, method: Method, args: Array[Any], result: Try[Any]): Any = {
        result match {
          case Failure(t) => throw t
          case Success(r: Int) => r * 2
        }
      }
    }
    val withDoubling = proxyfier.sandwich(multiply, doubler)
    assertEquals(9, multiply(3, 3))
    assertEquals(9 * 2, withDoubling(3, 3))
    assertEquals(121, multiply(11, 11))
    assertEquals(121 * 2, withDoubling(11, 11))
  }
  @Test def `interface` {
    trait Pure {
      def foo: Int
    }
      def handler(p: Pure, method: Method, args: Array[AnyRef]): Any = {
        assertEquals("foo", method.getName)
        42
      }
    val p = new Proxylicious[Pure]
    val pure = p.proxify(handler)
    assertEquals(42, pure.foo)
  }
  @Test def `retrying` {
    val multiply = new Multiply with ThirtiethTimesACharm
    val proxyfier = new util.RetryOnExceptionProxylicious[Arithmetic, IllegalStateException]
    val retryingMultiply = proxyfier.withRetry(multiply)
    try {
      multiply(5, 6)
      fail("Should fail on illegal state")
    } catch {
      case e: Exception => assertEquals(classOf[IllegalStateException], e.getClass)
    }
    assertEquals(42, retryingMultiply(6, 7))
  }

  trait Arithmetic {
    def apply(a: Int, b: Int): Int
  }
  class Multiply extends Arithmetic {
    def apply(a: Int, b: Int) = a * b
  }

  trait ThirtiethTimesACharm extends Arithmetic {
    private var invoCount = 0
    abstract override def apply(a: Int, b: Int) = {
      invoCount += 1
      if (invoCount < 30)
        throw new IllegalStateException
      else
        super.apply(a, b)
    }
  }

  @Test
  def `value class` {
    trait Values {
      def a: String
      def b: Int
      def c: Timestamp
    }
    class ValuesImpl(val a: String, val b: Int, val c: Timestamp) extends Values
    val now = new Timestamp
    val x = new ValuesImpl("nils", 42, now)
    val y = new ValuesImpl("nils", 42, now)
    val z = new ValuesImpl("nils", 43, now)
    assertNotEquals(x, y)
    assertNotEquals(x.hashCode, y.hashCode)
    val p = new Proxylicious[Values]
    val x2 = p.withEqualsHashCodeOverride(x)
    val y2 = p.withEqualsHashCodeOverride(y)
    val z2 = p.withEqualsHashCodeOverride(z)
    assertEquals(x2, y2)
    assertEquals(x2.hashCode, y2.hashCode)
    assertNotEquals(x2, z2)
    assertNotEquals(x2.hashCode, z2.hashCode)
  }

  @Test
  def `ToStringOverride` {
    trait Values {
      def a: String
      def b: Int
    }
    val values = new Values {
      val a = "Foo"
      val b = 42
    }
    val p = new Proxylicious[Values]
    val valuesP = p.withToStringOverride(values, "Values")
    assertEquals("Values(b=42,a=Foo)", valuesP.toString)
  }

}
