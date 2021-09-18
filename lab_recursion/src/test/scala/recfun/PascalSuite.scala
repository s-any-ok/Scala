package recfun

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

import scala.util.control.Breaks.{break, breakable}

@RunWith(classOf[JUnitRunner])
class PascalSuite extends AnyFunSuite {
  import Main.pascal
    test("pascal: col=0,row=2") {
      assert(pascal(0,2) === 1)
  }

    test("pascal: col=1,row=2") {
      assert(pascal(1,2) === 2)
  }

    test("pascal: col=1,row=3") {
      assert(pascal(1,3) === 3)
  }

    test("pascal: col=2,row=3") {
      assert(pascal(2,3) === 3)
  }

  for( w <- 0 to 10)
  {
    breakable {
      if (w == 2) break
      else
        test(s"pascal: col=0,row=$w") {
          assert(pascal(0,w) === 1)
        }
    }
  }

  for( w <- 1 to 10)
  {
    test(s"pascal: col=$w,row=$w") {
      assert(pascal(w,w) === 1)
    }
  }



}
