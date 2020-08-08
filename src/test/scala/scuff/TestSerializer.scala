package scuff

import org.junit._, Assert._

class TestSerializer {

  @Test def typed(): Unit = {
    val ser = new JavaSerializer[List[Int]]
    val orgList = 4 :: 5 :: 6 :: Nil
    val bytes = ser.encode(orgList)
    val newList = ser.decode(bytes)
    assertEquals(orgList, newList)
  }

  @Test def untyped(): Unit = {
    val ser = JavaSerializer
    val orgObj = java.util.UUID.randomUUID()
    val bytes = ser.encode(orgObj)
    val newObj = ser.decode(bytes)
    assertEquals(orgObj, newObj)
  }

}
