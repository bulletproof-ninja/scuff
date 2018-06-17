package scuff.concurrent

import org.junit._
import org.junit.Assert._
import scala.concurrent.Future

class TestImplicits {

  def futureInt: Future[Int] = Future successful 42

  @Test
  def flatten(): Unit = {
    val f: Future[Future[Int]] = Future successful futureInt
    val flat: Future[Int] = f.flatten
    assertEquals(42, flat.await)
  }

}
