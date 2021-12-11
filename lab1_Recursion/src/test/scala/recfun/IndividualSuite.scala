package recfun

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IndividualSuite extends AnyFunSuite {
  import Main.individual
    test("individual: k=8 i=1 x=11") {
      assert(individual()(11) === 8642950081920L)
  }

  test("individual: k=8 i=1 x=0") {
    assert(individual()(0) === 0)
  }

  test("individual: k=8 i=1 x=9") {
    assert(individual()(9) === 0)
  }

  test("individual: k=8 i=1 x=10") {
    assert(individual()(10) === -1)
  }
}
