package streams
import IndividualTask.{individual, individualPartialFunc}

class IndividualTaskSuite extends munit.FunSuite {

  val range = (-250 to 250)
  val list = range.filter(individualPartialFunc.isDefinedAt).map(individualPartialFunc)

  test("L.map(f) and L.count(f): multiply by 0 and count 0") {
    val result = list.map(x => x * 0).count(x => x == 0)
    assertEquals(result, 500)
  }

  test("L.filter(f) : filter < 0") {
    val result = list.filter(x => x < 0).length
    assertEquals(result, 0)
  }

  test("L.exists(f) : exists x < 132131231 and x < 0") {
    val result = list.exists(x => x < 132131231)
    val result2 = list.exists(x => x < 0)
    assertEquals(result, true)
    assertEquals(result2, false)
  }

  test("L.filterNot(f) : filterNot x%11 == 0") {
    val result = list.filterNot(x => x%11 == 0).length
    assertEquals(result, 218)
  }

  test("individualPartialFunc isDefinedAt(9)") {
    assertEquals(individualPartialFunc.isDefinedAt(9), true)
  }

  test("individualPartialFunc isDefinedAt(10)") {
    assertEquals(individualPartialFunc.isDefinedAt(10), false)
  }

  test("individualPartialFunc isDefinedAt(11)") {
    assertEquals(individualPartialFunc.isDefinedAt(11), true)
  }

  test("individual: k=8 i=1 x=11") {
    assert(individualPartialFunc(11) == 8642950081920L)
  }

  test("individual: k=8 i=1 x=0") {
    assert(individualPartialFunc(0) == 0)
  }

  test("individual: k=8 i=1 x=9") {
    assert(individualPartialFunc(9) == 0)
  }

  test("individual: k=8 i=1 x=10") {
    assert(individualPartialFunc(10) == -1)
  }

  import scala.concurrent.duration._
  override val munitTimeout: FiniteDuration = 10.seconds
}
