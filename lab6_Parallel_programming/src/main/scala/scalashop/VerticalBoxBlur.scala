package scalashop

import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks =30
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}

object VerticalBoxBlur extends VerticalBoxBlurInterface {
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for(
      r <- from until end;
      c <- 0 until src.height
      if r >= 0 && r < src.width
    ) yield {
      dst.update(r, c, boxBlurKernel(src, r, c, radius))
    }
  }

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val r = 0 to src.width by (src.width/Math.min(numTasks, src.width))
    val ranges = r.zip(r.tail)
    val tasks = ranges.map{ case (from, to) => task(blur(src, dst, from, to, radius)) }
    tasks.foreach(_.join)
  }
}
