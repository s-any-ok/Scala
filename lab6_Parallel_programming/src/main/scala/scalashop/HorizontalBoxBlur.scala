package scalashop

import org.scalameter._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for(
      r <- 0 until src.width;
      c <- from until end
      if c >= 0 && c < src.height
    ) yield {
      dst.update(r, c, boxBlurKernel(src, r, c, radius))
    }
  }

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val r = 0 to src.height by (src.height/Math.min(numTasks, src.height))
    val ranges = r.zip(r.tail)
    val tasks = ranges.map{ case (from, to) => task(blur(src, dst, from, to, radius)) }
    tasks.foreach(_.join)
  }

}
