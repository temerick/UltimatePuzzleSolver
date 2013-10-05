package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 11:45 AM
 */
object PuzzleSolver {
  def main(args: Array[String]) {

  }

}

case class RectangleStore(collection: Set[Rectangle])

object RectangleStreamBuilder {
  def getSquares(n: Int)(implicit rs: RectangleStore): Stream[Rectangle] = getRectangles(n,n)

  def getRectangles(n: Int,m: Int)(implicit rs: RectangleStore): Stream[Rectangle] = (n,m) match {
    case (1,1) => rs.collection.toStream
    case (x,y) if x >= y =>
      val xSplit: Int = x / 2

  }
}
