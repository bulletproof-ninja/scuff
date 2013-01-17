package scuff

import org.junit._
import org.junit.Assert._
import org.junit.Test

class TestSerializer {

  @Test def simple() {
    val ser = new JavaSerializer[List[Int]]
    val orgList = 4 :: 5 :: 6 :: Nil
    val bytes = ser.forth(orgList)
    val newList = ser.back(bytes)
    assertEquals(orgList, newList)
  }

}