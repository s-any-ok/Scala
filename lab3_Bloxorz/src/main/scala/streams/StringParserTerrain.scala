package streams

trait StringParserTerrain extends GameDef {

  val level: String

  def isCorrectPos(pos: Pos):Boolean =
    pos.row >= 0 && pos.col >= 0

  def isPosInTerrain(pos: Pos, levelVector: Vector[Vector[Char]]):Boolean =
    (pos.row < levelVector.length) && (pos.col < levelVector(pos.row).length)

  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean =
    pos => isCorrectPos(pos) && isPosInTerrain(pos, levelVector) && levelVector(pos.row)(pos.col).toString != "-"

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos =
    Pos(
      levelVector.indexWhere(_.indexOf(c) != -1), // row
      levelVector(levelVector.indexWhere(_.indexOf(c) != -1)).indexOf(c) // col(row)
    )

  def getVector = vector

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\r?\n").map(str => Vector(str: _*)).toIndexedSeq: _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
