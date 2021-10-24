package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object IndividualTask {

  def main(args: Array[String]) {
    println("IndividualTask")
    val range = (-250 to 250)
    val list = toList(range,individualPartialFunc)
    list.foreach(println)

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

  def toList(range: Seq[Int], f: PartialFunction[Int, BigInt]) =
  {
    for (x <- range.toList if f isDefinedAt(x)) yield f(x)
  }
}
