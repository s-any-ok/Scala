package scalashop

object IndividualRunner {

  def main(args: Array[String]): Unit = {
    //Individual.individualPartialFuncSimple(-250, 250, 10)
    Individual.individualPartialFuncTask(-250, 250, 10)
  }

}

object Individual {

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

  def individualPartialFuncTask(fromVal: Int, toVal: Int, numTasks: Int): Unit = {
    val ranges = fromVal to toVal by numTasks
    val tasks = ranges.map{ case v => task({
      println(individualPartialFunc(v))
    }) }
    tasks.foreach(_.join)
  }

  def individualPartialFuncSimple(fromVal: Int, toVal: Int, numTasks: Int): Unit = {
    val ranges = fromVal to toVal by numTasks
    ranges.foreach{v => {
      println(individualPartialFunc(v))
    } }
  }
}
