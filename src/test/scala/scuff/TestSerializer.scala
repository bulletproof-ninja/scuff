package scuff

import org.junit._
import org.junit.Assert._
import org.junit.Test

class TestSerializer {

  @Test def typed() {
    val ser = new JavaSerializer[List[Int]]
    val orgList = 4 :: 5 :: 6 :: Nil
    val bytes = ser.forth(orgList)
    val newList = ser.back(bytes)
    assertEquals(orgList, newList)
  }

  @Test def untyped() {
    val ser = JavaSerializer
    val orgObj = java.util.UUID.randomUUID()
    val bytes = ser.forth(orgObj)
    val newObj = ser.back(bytes)
    assertEquals(orgObj, newObj)
  }

}