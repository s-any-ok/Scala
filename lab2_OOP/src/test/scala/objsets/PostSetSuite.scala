package objsets

import scala.util.Random

class PostSetSuite extends munit.FunSuite {

  val set1 = new Empty()
  val empty = new Empty()
  val aPost = new Post("c", "c body", 7)
  val set2: PostSet = set1.incl(new Post("a", "a body", 20))
  val set3: PostSet = set2.incl(new Post("b", "b body", 20))
  val c = new Post("c", "c body", 7)
  val d = new Post("d", "d body", 9)
  val set4c: PostSet = set3.incl(c)
  val set4d: PostSet = set3.incl(d)
  val set5: PostSet = set4c.incl(d)

  def asSet(posts: PostSet): Set[Post] = {
    var res = Set[Post]()
    posts.foreach(res += _)
    res
  }

  def size(set: PostSet): Int = asSet(set).size

//  val alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
//  def randStr(n:Int) = (1 to n).map(_ => alpha(Random.nextInt(alpha.length))).mkString
//
//  def randNum(from:Int, to:Int) = Random.between(from, to + 1);
//
//  def randPost(nameLen:Int, postLen:Int, likesRange:Array[Int]) =
//    new Post(randStr(nameLen), randStr(postLen), randNum(likesRange(0), likesRange(1)))
//
//  def randPostSet(n:Int, startPostSet: PostSet = new Empty): PostSet =
//    if(n > 0) randPostSet(n - 1, startPostSet.incl(randPost(5, 10, Array(1, 100))))
//    else startPostSet

  test("filter: on empty set") {
    assertEquals(size(set1.filter(tw => tw.user == "a")), 0)
  }

  test("filter: a on set5") {
      assertEquals(size(set5.filter(tw => tw.user == "a")), 1)
  }

  test("filter: twenty on set5") {
      assertEquals(size(set5.filter(tw => tw.likes == 20)), 2)
  }

  test("union: set4c and set4d") {
      assertEquals(size(set4c.union(set4d)), 4)
  }

  test("union: with empty set1") {
      assertEquals(size(set5.union(set1)), 4)
  }

  test("union: with empty set2") {
      assertEquals(size(set1.union(set5)), 4)
  }

  test("descending: set5") {
    val trends = set5.descendingByLikes
    assert(!trends.isEmpty)
    assert(trends.head.user == "a" || trends.head.user == "b")
  }

//  test("randPostSet: create 200 posts") {
//    assertEquals(size(randPostSet(200)), 200)
//  }
//
//  test("union: 100 posts and 200 posts") {
//    assertEquals(size(randPostSet(200).union(randPostSet(100))), 300)
//  }

  test("incl: on empty") {
    assertEquals(empty.incl(aPost).contains(aPost), true)
    assertEquals(empty.isInstanceOf[empty.type], true)
    assertEquals(empty.incl(aPost).isInstanceOf[empty.type], false)
  }

//  test("GoogleVsApple") {
//    //println(size(GoogleVsApple.googleposts))
//    assertEquals(size(GoogleVsApple.googleposts), 38)
//  }
//
//  test("CEO in posts") {
//    assertEquals(size(GoogleVsApple.wordsInPosts(List("CEO"))), 8)
//  }
//
//  test("Samsung in posts") {
//    assertEquals(size(GoogleVsApple.wordsInPosts(List("Samsung"))), 12)
//  }
//
//  test("Startup and Iran in posts") {
//    assertEquals(size(GoogleVsApple.wordsInPosts(List("Startup", "Iran"))), 11)
//  }



  import scala.concurrent.duration._
  override val munitTimeout: FiniteDuration = 10.seconds

}
