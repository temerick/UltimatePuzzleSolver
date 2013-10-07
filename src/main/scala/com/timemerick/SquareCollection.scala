package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 4:41 PM
 */
case class SquareCollection(squares: Stream[SingleSquare]) {
  val ids: Seq[Int] = squares.map(_.id)

  def ++(that: SquareCollection): SquareCollection = SquareCollection((squares++that.squares).sortBy(_.id))

  def -(that: SquareCollection): SquareCollection = SquareCollection(squares.filterNot(that.squares.contains))
}

object SquareCollectionReader {
  val shapes = Seq(OutTriangle, OutTriangleInverse, InTriangle,
    InTriangleInverse, Oval, OvalInverse, Plus, PlusInverse)
  def getCollectionFromFile(filename: String): SquareCollection = {
    val lines = io.Source.fromInputStream(this.getClass.getResourceAsStream(filename)).getLines()
    val readSquares = lines.toSeq
      .filterNot(_.startsWith("#"))
      .map(_.split(",").map(i => shapes.filter(_.id == i.toInt).head))
      .zipWithIndex
      .map(x => new SingleSquare(x._1(0),x._1(1),x._1(2),x._1(3),x._2))
      .toStream
    println("Read in %s squares from file.".format(readSquares.length))
    SquareCollection(readSquares)
  }
}
