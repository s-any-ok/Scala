package objsets


class Post(val user: String, val text: String, val likes: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + likes + "]"
}

abstract class PostSet extends postsetInterface {
  def isEmpty: Boolean

  def isEmptyLikes: Int

  def filter(p: Post => Boolean): PostSet = this.filterAcc(p, new Empty)

  def filterAcc(p: Post => Boolean, acc: PostSet): PostSet

  def union(that: PostSet): PostSet

  def mostLiked: Post

  def descendingByLikes: PostList

  def incl(post: Post): PostSet

  def remove(post: Post): PostSet

  def contains(post: Post): Boolean

  def foreach(f: Post => Unit): Unit

}

class Empty extends PostSet {

  def isEmpty: Boolean = true
  def isEmptyLikes: Int = 0

  def union(that: PostSet): PostSet = that

  def mostLiked: Post = throw new NoSuchElementException

  def descendingByLikes: PostList = Nil

  def contains(post: Post): Boolean = false

  def incl(post: Post): PostSet = new NonEmpty(post, new Empty(), new Empty())

  def remove(post: Post): PostSet = this

  def foreach(f: Post => Unit): Unit = ()

  override def filterAcc(p: Post => Boolean, acc: PostSet): PostSet = acc
}

class NonEmpty(elem: Post, left: PostSet, right: PostSet) extends PostSet {

  def isEmpty: Boolean = false
  def isEmptyLikes: Int = mostLiked.likes

  def filterAcc(p: Post => Boolean, acc: PostSet): PostSet =
    right.filterAcc(p, left.filterAcc(p, if(p(elem)) acc.incl(elem) else acc))

  def union(that: PostSet): PostSet = left.union(right.union(that)).incl(elem)

  def mostLiked: Post = {
    if(left.isEmptyLikes > elem.likes || right.isEmptyLikes > elem.likes)
      if(right.isEmptyLikes > left.isEmptyLikes) right.mostLiked else left.mostLiked
    else elem

  }

  def descendingByLikes: PostList = {
    new Cons(mostLiked, remove(mostLiked).descendingByLikes)
  }

  def contains(x: Post): Boolean =
    if (x.text < elem.text)
      left.contains(x)
    else if (elem.text < x.text)
      right.contains(x)
    else true

  def incl(x: Post): PostSet =
    if (x.text < elem.text)
     new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text)
      new NonEmpty(elem, left, right.incl(x))
    else
      this

  def remove(tw: Post): PostSet =
    if (tw.text < elem.text)
      new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text)
      new NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Post => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait PostList {
  def head: Post

  def tail: PostList

  def isEmpty: Boolean


  def foreach(f: Post => Unit): Unit = {
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }
}

object Nil extends PostList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons(val head: Post, val tail: PostList) extends PostList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleposts: PostSet = PostReader.allposts.filter(post => google.exists(item => post.text.contains(item)))
  lazy val appleposts: PostSet = PostReader.allposts.filter(post => apple.exists(item => post.text.contains(item)))

  lazy val trending: PostList = (googleposts.union(appleposts)).descendingByLikes

  def wordsInPosts(words: List[String]): PostSet = PostReader.allposts.filter(post => words.exists(item => post.text.contains(item)))
}

object Main extends App {
  // Print the trending posts
  GoogleVsApple.trending foreach println
}
