package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 11:45 AM
 */
object PuzzleSolver {
  def main(args: Array[String]) {
    val mySquares = SquareCollectionReader.getCollectionFromFile("/starting_squares.txt")
    val x = System.nanoTime
    val allRectangles = RectangleStreamBuilder.getRectangles(2,1,mySquares)
    println("Length: "+allRectangles.toSet.size)
    allRectangles foreach println
    // println("RESULT: ")
    // println(allRectangles.head)
    println(((System.nanoTime - x)*1.0E-9) + " seconds elapsed.")
    // (0 to 100).foreach(i => { println("Rectangle #%s: \n==========".format(i));println(allRectangles(i)) } )
  }
}

object RectangleStreamBuilder {
  def getSquares(n: Int, sc: SquareCollection): Stream[Rectangular] = getRectangles(n, n, sc)

  def getRectangles(n: Int,m: Int,sc: SquareCollection): Stream[Rectangular] = (n,m) match {
    case (1,1) => sc.squares.toStream
    case (x,_) if x > 1 =>
      val divisions = binaryExpansion(x)
      //println("x divisions: "+divisions)
      val toPieceTogether = getRectangles(1,m,sc)
      //println("To Piece Together: \n"+toPieceTogether.head)
      val allStreams = buildStreams(divisions.length,toPieceTogether,TopToBottom)
      val streams = for { i <- 0 to (divisions.length-1); if divisions(i) == 1 } yield allStreams(i)
      streams.tail.foldLeft(streams.head)(attachStreamsTB)
    case (_,y) if y > 1 =>
      val divisions = binaryExpansion(m)
      //println("y divisions: "+divisions)
      val allStreams = buildStreams(divisions.length,sc,RightToLeft)
      //println("y allstreams length: "+allStreams.length)
      val streams = for { i <- 0 to (divisions.length-1); if divisions(i) == 1 } yield allStreams(i)
      //println("y streams length: "+streams.length)
      //println("-=-=-=-=-=-=-=-=\n"+streams.head.head+"\n-=-=-=-=-=-=-=-=-=-=-=-")
      // streams foreach (x => println("==========\n"+x.head))
      streams.tail.foldLeft(streams.head)(attachStreamsRL)
    case _ => throw new Exception("Improper input. n and m must both be positive integers.")
  }

  private def buildStreams(streamCount: Int, startStream: Stream[Rectangular],am: AttachmentMethod): Seq[Stream[Rectangular]] =
    buildStreams0(streamCount,Seq(startStream),am)

  private def buildStreams(streamCount: Int, sc: SquareCollection,am: AttachmentMethod): Seq[Stream[Rectangular]] =
    buildStreams0(streamCount,Seq(sc.squares.flatMap(_.allSymmetriesAsStream).toStream),am)

  private def buildStreams0(streamCount: Int, acc: Seq[Stream[Rectangular]],am: AttachmentMethod): Seq[Stream[Rectangular]] =
    acc.length match {
      case x: Int if x == streamCount => acc
      case _ =>
        // This doesn't make any sense.
        val toAttach = acc.head
        buildStreams0(streamCount, attachStreams(toAttach)(am) +: acc, am)
  }

  private def attachStreamsRL(a: Stream[Rectangular], b: Stream[Rectangular]): Stream[Rectangular] = attachStreams(a,b)(RightToLeft)
  private def attachStreamsTB(a: Stream[Rectangular], b: Stream[Rectangular]): Stream[Rectangular] = attachStreams(a,b)(TopToBottom)
  private def attachStreams(a: Stream[Rectangular], b: Stream[Rectangular]): AttachmentMethod => Stream[Rectangular] = {
    am => (for { x<-a.toIterator; y<-b; if (x.usedSquares.ids intersect y.usedSquares.ids).isEmpty } yield x.attach(y,am)).flatten.toStream
  }
  private def attachStreams(a: Stream[Rectangular]): AttachmentMethod => Stream[Rectangular] = {
    am => (for { x<-a; y<-a; if (x.usedSquares.ids intersect y.usedSquares.ids).isEmpty } yield x.attach(y,am)).flatten
  }

  private def binaryExpansion(x: Int): Seq[Int] = Integer.toBinaryString(x).split("").tail.map(_.toInt)
}
