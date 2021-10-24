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

  import scala.concurrent.duration._
  override val munitTimeout: FiniteDuration = 10.seconds
}
