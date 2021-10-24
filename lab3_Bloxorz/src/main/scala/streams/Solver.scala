package streams

trait Solver extends GameDef {

  def done(b: Block): Boolean = b.b1 == goal && b.b2 == goal

  def neighborsWithHistory(b: Block, history: List[Move]): LazyList[(Block, List[Move])] =
    for {
      (block, move) <- b.legalNeighbors.to(LazyList)
  } yield (block, move :: history)


  def newNeighborsOnly(neighbors: LazyList[(Block, List[Move])], explored: Set[Block]): LazyList[(Block, List[Move])] =
    for {
      (block, move)  <- neighbors
      if !explored.contains(block)
  } yield (block, move)

  def from(initial: LazyList[(Block, List[Move])], explored: Set[Block]): LazyList[(Block, List[Move])] =
    if (initial.isEmpty) LazyList.empty
    else {
      val next = for {
        (block, move)  <- initial
        neighbors <- newNeighborsOnly(neighborsWithHistory(block, move), explored)
      } yield neighbors
      initial #::: from(next, explored ++ next.map(_._1))
    }

  lazy val pathsFromStart: LazyList[(Block, List[Move])] = from(LazyList((startBlock, Nil)), Set.empty)

  lazy val pathsToGoal: LazyList[(Block, List[Move])] = pathsFromStart.filter(x => done(x._1))

  lazy val solution: List[Move] = if (pathsToGoal.isEmpty) Nil else pathsToGoal.map(x => x._2).head.reverse
}
