package streams

trait GameDef {

  /**
   * The case class `Pos` encodes positions in the terrain.
   *
   * IMPORTANT NOTE
   *  - The `row` coordinate denotes the position on the vertical axis
   *  - The `col` coordinate is used for the horizontal axis
   *  - The coordinates increase when moving down and right
   *
   * Illustration:
   *
   *     0 1 2 3   <- col axis
   *   0 o o o o
   *   1 o o o o
   *   2 o # o o    # is at position Pos(2, 1)
   *   3 o o o o
   *
   *   ^
   *   |
   *
   *   row axis
   */
  case class Pos(row: Int, col: Int) {
    /** The position obtained by changing the `row` coordinate by `d` */
    def deltaRow(d: Int): Pos = copy(row = row + d)

    /** The position obtained by changing the `col` coordinate by `d` */
    def deltaCol(d: Int): Pos = copy(col = col + d)
  }

  def startPos: Pos

  def goal: Pos

  type Terrain = Pos => Boolean

  def terrain: Terrain

  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  def startBlock: Block = Block(startPos, startPos)

  case class Block(b1: Pos, b2: Pos) {

    // checks the requirement mentioned above
    require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    /**
     * Returns a block where the `row` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def deltaRow(d1: Int, d2: Int) = Block(b1.deltaRow(d1), b2.deltaRow(d2))

    /**
     * Returns a block where the `col` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def deltaCol(d1: Int, d2: Int) = Block(b1.deltaCol(d1), b2.deltaCol(d2))


    /** The block obtained by moving left */
    def left = if (isStanding)             deltaCol(-2, -1)
               else if (b1.row == b2.row)  deltaCol(-1, -2)
               else                        deltaCol(-1, -1)

    /** The block obtained by moving right */
    def right = if (isStanding)            deltaCol(1, 2)
                else if (b1.row == b2.row) deltaCol(2, 1)
                else                       deltaCol(1, 1)

    /** The block obtained by moving up */
    def up = if (isStanding)               deltaRow(-2, -1)
             else if (b1.row == b2.row)    deltaRow(-1, -1)
             else                          deltaRow(-1, -2)

    /** The block obtained by moving down */
    def down = if (isStanding)             deltaRow(1, 2)
               else if (b1.row == b2.row)  deltaRow(1, 1)
               else                        deltaRow(2, 1)


    def neighbors: List[(Block, Move)] = List((up, Up), (right, Right), (down, Down), (left, Left))

    def legalNeighbors: List[(Block, Move)] = neighbors.filter(_._1.isLegal)

    def isStanding: Boolean = b1 == b2

    def isLegal: Boolean = terrain(b1) && terrain(b2)
  }
}
