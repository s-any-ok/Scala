package scalashop

import org.junit._

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
  @Test def `HorizontalBoxBlu / VerticalBoxBlur parBlur with 4 tasks should modify each pixel of the destination 4x4 image exactly once` = {
    val src = new Img(4, 4)
    for (x <- 0 until 4; y <- 0 until 4) src(x, y) = rgba(x, 3 * y, x + y, x)
    val dstHPar = new Img(4, 4)
    val dstVPar = new Img(4, 4)
    val numTasks = 4
    val radius = 3

    HorizontalBoxBlur.parBlur(src, dstHPar, numTasks, radius)
    VerticalBoxBlur.parBlur(src, dstVPar, numTasks, radius)

    for (cr <- 0 until src.width; rc <- 0 until src.width) {
      assert(dstHPar(rc, cr) == boxBlurKernel(src, rc, cr, radius))
      assert(dstVPar(cr, rc) == boxBlurKernel(src, cr, rc, radius))
    }
  }
  @Test def `HorizontalBoxBlu / VerticalBoxBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image` = {
    val src = new Img(4, 4)
    for (x <- 0 until 4; y <- 0 until 4) src(x, y) = rgba(x, 3 * y, x + y, x)
    val dstHPar = new Img(4, 4)
    val dstVPar = new Img(4, 4)
    val numTasks = 4
    val radius1 = 1
    val radius2 = 4

    HorizontalBoxBlur.parBlur(src, dstHPar, numTasks, radius1)
    VerticalBoxBlur.parBlur(src, dstVPar, numTasks, radius2)

    for (cr <- 0 until src.width; rc <- 0 until src.width) {
      assert(dstHPar(rc, cr) == boxBlurKernel(src, rc, cr, radius1))
      assert(dstVPar(cr, rc) == boxBlurKernel(src, cr, rc, radius2))
    }
  }
  @Test def `boxBlurKernel should correctly handle radius 0` = {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, y)

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) == rgba(x, y, x + y, y))
  }
  @Test def `boxBlurKernel should compute the averages of red, blue, green and alpha channels separately` = {
    val src = new Img(3, 3)
    for (x <- 0 until 3; y <- 0 until 3) src(x, y) = rgba(x, 3 * y, x + y, x)

    var accX = 0
    var accY = 0
    var count = 0
    var r = 0
    var g = 0
    var b = 0
    var a = 0
    while(accX <= 2) {
      while(accY <= 2) {
        val pixel = src.apply(accX, accY)
        r += red(pixel)
        g += green(pixel)
        b += blue(pixel)
        a += alpha(pixel)
        count += 1
        accY += 1
      }
      accX += 1
      accY = 0
    }
    assert(boxBlurKernel(src, 1, 1, 1) == rgba(r / count, g / count, b / count, a / count))
  }
}
