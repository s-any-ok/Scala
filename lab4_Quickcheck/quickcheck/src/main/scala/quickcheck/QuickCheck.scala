package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    j <- oneOf(const(empty), genHeap)
  } yield insert(i, j)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("#1-1") = forAll { (x: Int, y: Int) =>
    val h1 = insert(x, empty)
    val h2 = insert(y, h1)
    findMin(h2) == Math.min(x, y)
  }

  property("#1-2") = forAll { x: Int =>
    val h1 = insert(x, empty)
    val h2 = deleteMin(h1)
    h2 == empty
  }

  def isSorted(xs: List[Int]) = (xs, xs.tail).zipped.forall(_ <= _)
  def removeAll(heap: H): List[Int] =
    if (isEmpty(heap)) Nil
    else {
      val min = findMin(heap)
      val h = deleteMin(heap)
      min :: removeAll(h)
  }

  property("#1-3") = forAll { h: H =>
    val list = removeAll(h)
    isSorted(list)
  }

  property("#1-4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("#2-1") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val h3 = deleteMin(h2)
    val h4 = deleteMin(h3)
    isEmpty(h4)
  }

  property("#2-2") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) a+1 else a // 2147483647 + 1 == -2147483648
    val h1 = insert(b+1, insert(b, empty))
    val h2 = deleteMin(h1)
    h2 == insert(b+1, empty)
  }

  @tailrec
  final def heapEquals(h1: H, h2: H): Boolean = (h1, h2) match {
    case (Nil, Nil) => true
    case _ if (isEmpty(h1) || isEmpty(h2)) =>  false
    case (hs1, hs2) => {
      findMin(hs1) == findMin(hs2) && heapEquals(deleteMin(hs1), deleteMin(hs2))
    }
  }

  property("#2-3") = forAll { (h1: H, h2: H) =>
    !isEmpty(h1) ==> {
      val minH1 = findMin(h1)
      val h1Mod = deleteMin(h1)
      val h2Mod = insert(minH1, h2)
      heapEquals(meld(h1, h2), meld(h1Mod, h2Mod))
    }
  }

  property("#2-4") = forAll { (h1: H, h2: H) =>
    heapEquals(meld(h1, h2), meld(h2, h1))
  }

  property("#2-5") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val h3 = deleteMin(h2)
    findMin(h3) == Math.max(a, b)
  }

  def individual(k: BigInt = 8, i: BigInt = 1)(x: BigInt):BigInt = {
    if(x == 10) -1
    else if (x < 10) 0
    else if (i <= k) i*x * individual(k, i + 1)(x)
    else 1
  }

  def individualPartialFunc = new PartialFunction[Int, BigInt] {
    def apply(v1: Int): BigInt = {
      individual()(v1)
    }

    override def isDefinedAt(x: Int): Boolean = (x != 10)
  }

  def individualPartialFuncLift(x: Int) = {
    individualPartialFunc.lift(x).get
  }

  property("IndividualPartialFuncLift - #1") = forAll { (x: Int) =>
    if(x == 10) individualPartialFuncLift(x) == -1
    if (x < 10) individualPartialFuncLift(x) == 0
    else if(x > 10) individualPartialFuncLift(x) > 9
    else individualPartialFuncLift(x) == 1
  }

  property("IndividualPartialFuncLift - #2") = forAll { (x: Int) =>
    individualPartialFuncLift(x) >= -1
  }
}
